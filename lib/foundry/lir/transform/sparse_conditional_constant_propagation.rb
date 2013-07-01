module Foundry
  class LIR::Transform::SparseConditionalConstantPropagation < LIR::Processor
    # See http://www.cs.rice.edu/~keith/512/2011/Lectures/L19-SCCP-1up.pdf
    def run_on_function(translator, func)
      @updated       = false

      @cfg_worklist  = [ func.entry ].to_set
      @cfg_reachable = [].to_set # `mark' in the slides
      @ssa_worklist  = [].to_set

      @values = Hash.new do |values, value|
        if value.constant?
          value
        else
          values[value] = :TOP
        end
      end

      func.arguments.each do |arg|
        @values[arg] = :BOT
      end

      until @cfg_worklist.empty? && @ssa_worklist.empty?
        until @cfg_worklist.empty?
          block = @cfg_worklist.first
          @cfg_worklist.delete block

          @cfg_reachable.add block

          block.each do |insn|
            v = evaluate(insn)
            if v.nil?
              p insn
              raise
            end
            @values[insn] = v

            insn.each_use do |use|
              if @cfg_reachable.include? use.basic_block
                @ssa_worklist.add use
              end
            end
          end
        end

        until @ssa_worklist.empty?
          insn = @ssa_worklist.first
          @ssa_worklist.delete insn

          new_value = evaluate(insn)

          if @values[insn] != new_value
            @values[insn] = new_value

            insn.each_use do |use|
              if @cfg_reachable.include? use.basic_block
                @ssa_worklist.add use
              end
            end
          end
        end
      end

      @values.each do |insn, value|
        unless [:TOP, :BOT].include?(value)
          insn.replace_all_uses_with value
          insn.remove
        end
      end

      @updated
    end

    def evaluate(insn)
      case insn
      when LIR::PhiInsn
        insn.operands.values.reduce(:TOP) do |conj, op|
          if conj == :TOP
            # TOP ^ x = x
            @values[op]
          elsif conj == @values[op]
            # Ci ^ Cj = Ci  | Ci = Cj
            conj
          else
            # Ci ^ Cj = BOT | Ci ≠ Cj
            return :BOT
          end
        end

      when LIR::BranchInsn
        schedule_block insn.target

        :TOP

      when LIR::BranchIfInsn
        cond_value = @values[insn.condition]

        if cond_value == :TOP
          # Do nothing
        elsif cond_value == :BOT
          @cfg_worklist.merge [ insn.true_target, insn.false_target ]
        else
          if falseful? cond_value
            target = insn.false_target
          else
            target = insn.true_target
          end

          insn.replace_with(LIR::BranchInsn.new([ target ]))
          @ssa_worklist.delete insn

          schedule_block target

          @updated = true
        end

        :TOP

      when LIR::TupleInsn
        if insn.operands.all? &:constant?
          LIR::Constant.new(
              insn.type,
              VI.new_tuple(insn.operands.map(&:value)))
        else
          :BOT
        end

      when LIR::TupleRefInsn
        tuple     = insn.tuple
        tuple_idx = insn.index

        if insn.tuple.constant?
          Foundry.constant(insn.tuple.value[tuple_idx])
        elsif insn.tuple.is_a?(LIR::TupleInsn)
          insn.tuple.operands[tuple_idx]
        else
          :BOT
        end

      when LIR::TupleSizeInsn
        tuple    = insn.tuple
        tuple_ty = tuple.type

        if !tuple_ty.variable?
          Foundry.constant(
              VI.new_integer(tuple_ty.size))
        else
          :BOT
        end

      when LIR::TupleBiggerInsn
        tuple    = insn.tuple
        tuple_ty = tuple.type

        if !tuple_ty.variable?
          tuple_ty.size > insn.min_size ?
              Foundry.constant(VI::TRUE) :
              Foundry.constant(VI::FALSE)
        else
          :BOT
        end

      when LIR::TupleConcatInsn
        if insn.operands.all? &:constant?
          Foundry.constant(VI.new_tuple(insn.operands.map(&:value).reduce(:+)))
        else
          :BOT
        end

      when LIR::LutInsn
        if insn.operands.all? &:constant?
          LIR::Constant.new(
              insn.type,
              VI.new_lut(Hash[
                insn.pairs.map do |key, value|
                  [key.value.to_sym, value.value]
                end]))
        else
          :BOT
        end

      # when LIR::IntegerOpInsn
      #   if insn.operands.all? &:constant?

      #   else
      #     :BOT
      #   end

      when LIR::IsAInsn
        if insn.klass.constant? && !insn.object.type.variable?
          if insn.object.type == Type.klass(insn.klass.value)
            Foundry.constant(VI::TRUE)
          else
            Foundry.constant(VI::FALSE)
          end
        else
          :BOT
        end

      when LIR::ReifyInsn
        if insn.operands.all? &:constant?
          klass               = insn.klass.value
          parameters          = klass.parameters.to_a.map(&:to_sym)
          specializations     = insn.specializations.value
          specialization_keys = specializations.keys.to_a

          if (invalid_params = (specialization_keys - parameters)).empty?
            Foundry.constant(klass.reify(specializations))
          else
            invalid_params_s = invalid_params.map(&:inspect).join(', ')
            raise LIR::AnalysisError, "Class #{klass} cannot be " +
                                      "specialized by #{invalid_params_s}"
          end
        else
          :BOT
        end

      when LIR::ConstRefInsn
        cref = insn.cref

        if cref.constant?
          cref.value.each do |elem|
            if elem.const_defined?(insn.constant)
              return Foundry.constant(elem.const_get(insn.constant))
            end
          end

          :TOP
        else
          :BOT
        end

      when LIR::ConstFetchInsn
        scope = insn.scope

        if scope.constant?
          if scope.value.const_defined?(insn.constant)
            return Foundry.constant(scope.value.const_get(insn.constant))
          end

          :TOP
        else
          :BOT
        end

      else
        :BOT
      end
    end

    def schedule_block(block)
      unless @cfg_reachable.include? block
        @cfg_worklist.add block
      end
    end

    def falseful?(value)
      value_ty = value.type

      value_ty == Type.of(VI::NIL) ||
          value_ty == Type.of(VI::FALSE)
    end
  end
end

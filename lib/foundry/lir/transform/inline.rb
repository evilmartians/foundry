module Foundry
  class LIR::Transform::Inline < LIR::Processor
    def run_on_function(translator, func)
      updated      = false

      inline_queue = []

      func.each_instruction(LIR::InvokeInsn) do |insn|
        next unless insn.callee.constant?

        target_func = translator.lir_module[insn.callee.value]

        if func != target_func && should_inline?(target_func)
          inline_queue << [insn, target_func]
        end
      end

      inline_queue.each do |(invoke_insn, func_to_inline)|
        func.instrument do |i|
          i.transform_start("Inline #{func_to_inline.name}")
        end

        inlined_func  = func_to_inline.dup
        inlined_entry = inlined_func.entry
        return_insns  = inlined_func.
            each_instruction(LIR::ReturnInsn, LIR::ReturnValueInsn).
            to_a

        block_after_inlined = LIR::BasicBlock.new([], "exit.#{inlined_func.name}")
        func.add block_after_inlined

        invoke_insn.basic_block.
              splice(invoke_insn).each do |spliced_insn|
          block_after_inlined.append spliced_insn
        end

        inlined_func.each_basic_block do |block|
          block.each do |insn|
            insn.basic_block = block # FIXME remove
            insn.name = "i.#{insn.name}"
          end

          block.name = "i.#{block.name}"
          func.add block
        end

        value_deps = {}

        return_insns.each do |ret_insn|
          unless ret_insn.value_type == Type.bottom
            value_deps[ret_insn.basic_block] = ret_insn.value
          end

          branch_insn = LIR::BranchInsn.new([ block_after_inlined ])
          ret_insn.replace_with branch_insn
        end

        inlined_func.arguments.
            zip(invoke_insn.arguments).
            each do |inlined_arg, passed_arg|
              inlined_arg.replace_all_uses_with passed_arg
            end

        if value_deps.count > 1
          phi_insn = LIR::PhiInsn.new(Type.variable, value_deps)
          block_after_inlined.prepend(phi_insn)

          invoke_insn.replace_all_uses_with phi_insn
        elsif value_deps.count == 1
          invoke_insn.replace_all_uses_with value_deps.values.first
        end

        branch_insn = LIR::BranchInsn.new([ inlined_entry ])
        invoke_insn.basic_block.insert(invoke_insn, branch_insn)

        invoke_insn.remove

        updated = true
      end

      updated
    end

    def should_inline?(function)
      inline = false

      inline ||= function.
          each_instruction(LIR::ReifyInsn, LIR::AllocateInsn).
          any? do |insn|
        insn.operands.any? do |operand|
          !operand.constant?
        end
      end

      inline ||= function.
          each_instruction(LIR::IsAInsn).
          any? do |insn|
        !insn.klass.constant?
      end

      inline
    end
  end
end

module Foundry
  class LIR::Transform::BindingSimplification < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::BindingInsn) do |binding|
        optimize    = true
        need_next   = false

        store_count = Hash.new { |h, k| h[k] = 0 }
        store_insn  = Hash.new
        load_insns  = Hash.new { |h, k| h[k] = [] }

        binding.each_use do |insn|
          case insn
          when LIR::ClosureInsn, LIR::BindingInsn
            optimize = false
            break

          when LIR::LvarStoreInsn
            if insn.depth == 0
              store_count[insn.variable] += 1
              store_insn[insn.variable] = insn
            else
              need_next = true
            end

          when LIR::LvarLoadInsn
            if insn.depth == 0
              load_insns[insn.variable] << insn
            else
              need_next = true
            end
          end
        end

        next unless optimize

        store_count.each do |var, count|
          if count == 1
            value = store_insn[var].value
            store_insn[var].remove

            load_insns[var].each do |insn|
              insn.replace_all_uses_with value
              insn.remove
            end

            binding.variables.delete var
            binding.type.variables.delete var

            updated = true
          end
        end

        if binding.variables.empty?
          binding.remove
        elsif !need_next && binding.next.type != LIR.void
          binding.next = LIR.void_value
          binding.reset_type!
        end
      end

      updated
    end
  end
end
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
            store_insn[var].erase

            load_insns[var].each do |insn|
              insn.replace_all_uses_with value
              insn.erase
            end

            binding.remove_variable var

            updated = true
          end
        end

        if !need_next
          if binding.type.variables.empty?
            binding.remove
          elsif !binding.next.nil?
            binding.remove_next
          end
        end
      end

      updated
    end
  end
end

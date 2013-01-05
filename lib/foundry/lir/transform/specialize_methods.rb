module Foundry
  class LIR::Transform::SpecializeMethods
    def transform(translator)
      updated = false

      translator.each_function do |func|
        func.each_instruction(LIR::InvokeInsn) do |insn|
          if insn.callee.constant?
            generic_fun = translator.lir_module[insn.callee.value]
            specialized_fun =
                translator.specialize(generic_fun, insn.arguments.map(&:type))

            if specialized_fun != generic_fun
              insn.callee = specialized_fun.to_value

              updated = true
            end
          end
        end
      end

      translator if updated
    end
  end
end
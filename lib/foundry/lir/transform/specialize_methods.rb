module Foundry
  class LIR::Transform::SpecializeMethods < LIR::Processor
    def run_on_function(translator, func)
      updated = false

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

      updated
    end
  end
end
module Foundry
  class LIR::Transform::ReturnTypeInference < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      returns = func.each_instruction(LIR::ReturnInsn,
                                      LIR::ReturnValueInsn).to_a

      if returns.count == 1
        return_ty = returns.first.value_type

        if func.return_type != return_ty
          func.return_type = return_ty

          updated = true
        end
      end

      func.each_instruction(LIR::InvokeInsn) do |insn|
        if insn.callee.constant?
          called_fun = translator.lir_module[insn.callee.value]
          return_ty  = called_fun.return_type

          next if return_ty.variable?

          if insn.type != return_ty
            func.replace_type_with insn.type, return_ty

            updated = true
          end
        end
      end

      updated
    end
  end
end

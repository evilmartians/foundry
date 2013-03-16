module Foundry
  class LIR::Transform::ReturnTypeInference < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      returns = func.each_instruction(LIR::ReturnInsn).to_a
      if returns.count == 1
        value_ty = returns.first.value.type

        if value_ty && func.return_type != value_ty
          p 'FF was', func.return_type, value_ty
          func.return_type = value_ty

          updated = true
        end
      end

      func.each_instruction(LIR::InvokeInsn) do |insn|
        if insn.callee.constant?
          called_fun = translator.lir_module[insn.callee.value]
          return_ty  = called_fun.return_type

          next unless return_ty

          unless insn.type == return_ty
            p 'INV was', insn.type, return_ty
            insn.type = return_ty

            updated = true
          end
        end
      end

      updated
    end
  end
end
module Foundry
  class LIR::Transform::SpecializeMethods < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::InvokeInsn) do |insn|
        next unless insn.callee.constant?

        generic_fun  = translator.lir_module[insn.callee.value]
        arguments_ty = insn.arguments.map(&:type)

        translator.specialize(generic_fun,
              arguments_ty) do |specialized_fun|

          specialized_fun.arguments.each_with_index do |arg, index|
            arg.type = arguments_ty[index]
          end

          insn.callee = specialized_fun.to_value

          updated = true
        end
      end

      func.each_instruction(LIR::ClosureInsn) do |insn|
        next unless insn.callee.constant?

        generic_fun = translator.lir_module[insn.callee.value]
        binding_ty  = insn.binding.type

        translator.specialize(generic_fun,
              binding_ty) do |specialized_fun|

          self_arg, = specialized_fun.arguments

          self_arg.each_use do |insn|
            if insn.is_a?(LIR::IvarLoadInsn) &&
                  insn.variable.constant? &&
                  insn.variable.value.to_sym == :@binding

              insn.type = binding_ty

              insn.each_use &:reset_type!

              break
            end
          end

          insn.callee = specialized_fun.to_value

          updated = true
        end
      end

      updated
    end
  end
end
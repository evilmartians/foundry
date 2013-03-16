module Foundry
  class LIR::Transform::SpecializeMethods < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::InvokeInsn) do |insn|
        next unless insn.callee.constant?

        generic_fun  = translator.lir_module[insn.callee.value]
        arguments_ty = insn.arguments.map(&:type)

        insn.callee = translator.specialize(generic_fun,
                            arguments_ty) do |specialized_fun|

          specialized_fun.
              arguments.
              map.with_index do |arg, index|
                arg.type.specialize(arguments_ty[index])
              end.
              reduce(:merge).
              each do |type_var, replacement|
                specialized_fun.replace_type_with(type_var, replacement)
              end

          updated = true
        end
      end
=begin
      func.each_instruction(LIR::ClosureInsn) do |insn|
        next unless insn.callee.constant?

        generic_fun = translator.lir_module[insn.callee.value]
        binding_ty  = insn.binding.type

        insn.callee = translator.specialize(generic_fun,
                            binding_ty) do |specialized_fun|

          self_arg, = specialized_fun.arguments

          self_arg.each_use do |insn|
            if insn.is_a?(LIR::IvarLoadInsn) &&
                  insn.variable.constant? &&
                  insn.variable.value.to_sym == :@binding

              insn.type = binding_ty

              break
            end
          end

          updated = true
        end
      end
=end
      updated
    end
  end
end

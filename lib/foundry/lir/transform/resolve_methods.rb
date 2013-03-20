module Foundry
  class LIR::Transform::ResolveMethods < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::ResolveMethodInsn) do |insn|
        if insn.receiver.type &&
              insn.method.constant?

          type, method = insn.receiver.type, insn.method.value

          next if type.variable? || type == Type.top

          klass = type.to_klass

          if klass.method_defined?(method)
            proc     = klass.instance_method(method)
            function = translator.add_method proc

            insn.replace_with(function.to_value)

            updated = true
          end
        end
      end

      updated
    end
  end
end

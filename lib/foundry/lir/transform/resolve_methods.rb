module Foundry
  class LIR::Transform::ResolveMethods < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::ResolveMethodInsn) do |insn|
        if insn.receiver.type &&
              insn.method.constant?
          type, method = insn.receiver.type, insn.method.value

          if type.method_defined?(method)
            proc     = type.instance_method(method)
            function = translator.add_method proc

            insn.replace_with(function.to_value)
          else
            raise LIR::AnalysisError, "Undefined method #{method} for #{type}"
          end

          updated = true
        end
      end

      updated
    end
  end
end
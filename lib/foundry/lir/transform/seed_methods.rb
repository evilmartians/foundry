module Foundry
  class LIR::Transform::SeedMethods
    def transform(translator)
      updated = false

      translator.each_function do |func|
        func.each_instruction(LIR::ResolveMethodInsn) do |insn|
          if insn.receiver.type &&
                insn.method.constant?
            type, method = insn.receiver.type, insn.method.value

            if type.method_defined?(method)
              proc     = type.instance_method(method)
              function = translator.seed_method proc

              insn.replace_with(function.to_value)
            else
              raise LIR::AnalysisError, "Undefined method #{method} for #{type}"
            end

            updated = true
          end
        end
      end

      translator if updated
    end
  end
end
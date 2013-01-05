module Foundry
  class LIR::Transform::MethodSeeder
    def transform(translator)
      updated = false

      translator.each_function do |func|
        func.each_instruction(LIR::InvokeMethodInsn) do |insn|
          if insn.receiver.type &&
                insn.method.constant?
            type, method = insn.receiver.type, insn.method.value

            if type.method_defined?(method)
              proc = type.instance_method(method)

              unless translator.has_method? proc
                translator.seed_method proc
                updated = true
              end
            else
              raise LIR::AnalysisError, "Undefined method #{method} for #{type}"
            end
          end
        end
      end

      translator if updated
    end
  end
end
module Foundry
  class LIR::Transform::LocalTypeInference < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::LvarStoreInsn) do |insn|
        value_ty   = insn.value.type
        binding_ty = insn.binding.type

        if value_ty && value_ty.monotype?
          variable_ty = binding_ty.type_at(insn.depth, insn.variable)

          if variable_ty.nil?
            binding_ty.set_type_at(insn.depth, insn.variable, value_ty)

            updated = true
          end
        end
      end

      func.each_instruction(LIR::PhiInsn) do |insn|
        types      = insn.operands.values.map(&:type)
        uniq_types = types.uniq

        if uniq_types.one?
          insn.type = uniq_types.first
        elsif uniq_types.all? && uniq_types.all?(&:monotype?)
          raise LIR::AnalysisError, "ambiguous phi node"
        end
      end

      updated
    end
  end
end
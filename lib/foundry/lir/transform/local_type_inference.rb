module Foundry
  class LIR::Transform::LocalTypeInference < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::LvarStoreInsn) do |insn|
        value_ty   = insn.value.type
        binding_ty = insn.binding.type
        slot_ty    = binding_ty.type_at(insn.depth, insn.variable)

        if slot_ty.variable? && slot_ty != value_ty
          func.replace_type_with(slot_ty, value_ty)

          updated = true
        end
      end

      func.each_instruction(LIR::PhiInsn) do |insn|
        types      = insn.operands.values.map(&:type)
        uniq_types = types.uniq

        if uniq_types.one?
          insn.type = uniq_types.first
        elsif uniq_types.reject(&:variable?).count > 1
          raise LIR::AnalysisError, "ambiguous phi node"
        end
      end

      updated
    end
  end
end

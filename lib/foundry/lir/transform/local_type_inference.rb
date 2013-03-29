module Foundry
  class LIR::Transform::LocalTypeInference < LIR::Processor
    def run_on_function(translator, func)
      updated = false

      func.each_instruction(LIR::LvarStoreInsn) do |insn|
        value_ty   = insn.value.type
        binding_ty = insn.binding.type
        slot_ty    = binding_ty.type_at(insn.depth, insn.variable)

        if slot_ty.variable? &&
              slot_ty != value_ty &&
              value_ty != Type.top

          func.replace_type_with(slot_ty, value_ty)

          updated = true
        end
      end

      func.each_instruction(LIR::CheckTypeInsn) do |insn|
        if insn.expected_type.constant? && insn.type.variable?
          func.replace_type_with(insn.type,
                      Type.klass(insn.expected_type.value))
        end

        if !insn.expression.type.variable? &&
                insn.type == insn.expression.type

          insn.replace_with(insn.expression)

          updated = true
        end
      end

      func.each_instruction(LIR::SpecializationInsn) do |insn|
        if !insn.object.type.variable? &&
              insn.object.type != Type.top &&
              insn.specialization_name.constant?

          dependent_type = insn.object.type.specializations[
              insn.specialization_name.value.to_sym]
          constant_value = Foundry.constant(dependent_type.value)

          insn.replace_with constant_value
        end
      end

      func.each_instruction(LIR::PhiInsn) do |insn|
        types      = insn.operands.values.map(&:type)
        uniq_types = types.uniq

        if uniq_types.one? &&
              uniq_types.first != Type.top

          insn.type = uniq_types.first

          updated = true
        end
      end

      updated
    end
  end
end

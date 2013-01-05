module Foundry
  class LIR::Transform::DeadCodeElimination
    def transform(translator)
      updated = false

      translator.each_function do |func|
        worklist = func.each_instruction.to_set

        until worklist.empty?
          insn = worklist.first
          worklist.delete insn

          unless insn.used? || insn.has_side_effects?
            worklist.merge insn.each_operand.reject(&:constant?)
            insn.remove

            updated = true
          end
        end
      end

      translator if updated
    end
  end
end
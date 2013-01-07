module Foundry
  class LIR::Transform::DeadCodeElimination < LIR::Processor
    def run_on_function(translator, func)
      updated  = false
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

      updated
    end
  end
end
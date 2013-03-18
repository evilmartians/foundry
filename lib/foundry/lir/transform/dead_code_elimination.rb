module Foundry
  class LIR::Transform::DeadCodeElimination < LIR::Processor
    def run_on_function(translator, func)
      updated      = false

      cfg_worklist = func.each_basic_block.to_set
      ssa_worklist = func.each_instruction.to_set

      until ssa_worklist.empty? && cfg_worklist.empty?
        until cfg_worklist.empty?
          block = cfg_worklist.first
          cfg_worklist.delete block

          phi_uses, branch_uses = block.each_use.
                partition { |use| use.is_a?(LIR::PhiInsn) }

          unless func.entry == block || branch_uses.any? || block.exits?
            phi_uses.each do |phi|
              phi.operands.delete block

              if phi.operands.one?
                _, value = phi.operands.first
                phi.replace_all_uses_with value
                phi.remove
              end
            end

            block.each_instruction do |insn|
              add_instruction_operands_to_worklist(ssa_worklist, insn)
            end

            cfg_worklist.merge block.successors

            func.remove block

            updated = true
          end
        end

        until ssa_worklist.empty?
          insn = ssa_worklist.first
          ssa_worklist.delete insn

          unless insn.used? ||
                    insn.has_side_effects? ||
                    insn.terminator?
            add_instruction_operands_to_worklist(ssa_worklist, insn)
            insn.remove

            updated = true
          end
        end
      end

      updated
    end

    protected

    def add_instruction_operands_to_worklist(ssa_worklist, insn)
      possibly_dead_insns = insn.
          each_operand.select do |operand|
            operand.is_a? LIR::Instruction
          end

      ssa_worklist.merge possibly_dead_insns
    end
  end
end

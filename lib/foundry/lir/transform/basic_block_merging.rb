module Foundry
  class LIR::Transform::BasicBlockMerging < LIR::Processor
    def run_on_function(translator, func)
      worklist  = [ func.entry ].to_set
      reachable = [].to_set

      until worklist.empty?
        basic_block = worklist.first
        worklist.delete basic_block
        reachable.add basic_block

        if basic_block.successors.count == 1
          succ = basic_block.successors.first

          if succ.predecessors.count == 1 &&
                !basic_block.terminator.has_side_effects?
            basic_block.terminator.remove

            succ.each_instruction do |insn|
              basic_block.append insn
              insn.basic_block = basic_block
            end

            succ.replace_all_uses_with basic_block

            func.remove succ
          end
        end

        worklist.merge(basic_block.successors.to_set - reachable)
      end

      false
    end
  end
end

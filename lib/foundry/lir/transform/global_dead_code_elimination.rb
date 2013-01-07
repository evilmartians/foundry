module Foundry
  class LIR::Transform::GlobalDeadCodeElimination
    def initialize(keep_names)
      @keep_names = keep_names
    end

    def run(translator)
      mod      = translator.lir_module
      worklist = Set.new
      seen     = Set.new

      @keep_names.each do |name|
        worklist.add mod[name] if mod.include? name
      end

      until worklist.empty?
        func = worklist.first
        worklist.delete func
        seen.add func

        func.each_instruction do |insn|
          case insn
          when LIR::InvokeInsn, LIR::ClosureInsn
            next_name = insn.callee
          else
            next
          end

          if next_name.constant?
            next_func = mod[next_name.value]
            worklist.add next_func unless seen.include? next_func
          end
        end
      end

      updated = false

      mod.each do |func|
        if !seen.include? func
          mod.remove func.name
          updated = true
        end
      end

      updated
    end
  end
end
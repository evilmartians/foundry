module Foundry
  class LIR::ResolveInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :object
      s.operand :method, VI::Symbol
    end

    def type
      VI::Proc
    end
  end
end
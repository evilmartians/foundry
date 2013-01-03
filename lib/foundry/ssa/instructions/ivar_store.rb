module Foundry
  class SSA::IvarStoreInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :object
      s.operand :variable, VI::Symbol
      s.operand :value
    end
  end
end
module Foundry
  class LIR::IvarLoadInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :object
      s.operand :variable, Type.klass(VI::Symbol)
    end
  end
end

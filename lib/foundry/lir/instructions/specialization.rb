module Foundry
  class LIR::SpecializationInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :object
      s.operand :specialization_name
    end
  end
end

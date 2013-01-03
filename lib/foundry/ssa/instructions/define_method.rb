module Foundry
  class SSA::DefineMethodInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass
      s.operand :method,  VI::Symbol
      s.operand :body,    VI::Proc
    end
  end
end
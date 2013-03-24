module Foundry
  class LIR::IntegerConvInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :value
      s.operand :signedness
      s.operand :width
    end

    def type
      if signedness.constant? && width.constant?
        Type.klass(VI::Machine_Integer.reify(
            VMSymbol.new(:signed) => signedness.value,
            VMSymbol.new(:width)  => width.value))
      else
        Type.top
      end
    end
  end
end

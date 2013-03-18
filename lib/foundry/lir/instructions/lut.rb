module Foundry
  class LIR::LutInsn < Furnace::SSA::Instruction
    def type
      if pairs.all? { |k, v| k.constant? }
        Type::LookupTable.new(Hash[
            pairs.map do |key, value|
              [ key.value.to_sym, value.type ]
            end])
      else
        Type.top
      end
    end

    def pairs
      operands.each_slice(2)
    end
  end
end

module Foundry
  class LIR::TupleSliceInsn < Furnace::SSA::Instruction
    attr_accessor :from, :to

    syntax do |s|
      s.operand :tuple, Type.klass(VI::Tuple)
    end

    def initialize(basic_block, from, to, operands=[], name=nil)
      @from, @to = from.to_i, to.to_i

      super(basic_block, operands, name)
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.text @from, '..', @to, ','
    end

    def range
      if tuple.type.size
        map_index(@from)..map_index(@to)
      end
    end

    def type
      TupleType.new(tuple.type.element_types[@from..@to])
    end

    protected

    def map_index(idx)
      if tuple.type.size
        if idx < 0
          tuple.type.size + idx
        else
          idx
        end
      end
    end
  end
end

module Foundry
  class LIR::TupleSliceInsn < Furnace::SSA::Instruction
    attr_accessor :from, :to

    syntax do |s|
      s.operand :tuple
    end

    def initialize(from, to, operands=[], name=nil)
      @from, @to = from.to_i, to.to_i

      super(operands, name)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text @from, '..', @to, ','
    end

    def range
      if tuple.type.size
        map_index(@from)..map_index(@to)
      end
    end

    def type
      if @opreands && !tuple.type.variable?
        Type::Tuple.new(tuple.type.element_types[@from..@to])
      else
        Type.top
      end
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

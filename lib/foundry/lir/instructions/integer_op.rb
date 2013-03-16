module Foundry
  class LIR::IntegerOpInsn < Furnace::SSA::Instruction
    OPERATIONS = [ :+, :-, :*, :/, :%, :<, :<=, :>, :>=, :==, :!= ]

    attr_accessor :operation

    syntax do |s|
      s.operand :left#,  Type.klass(VI::Integer)
      s.operand :right#, Type.klass(VI::Integer)
    end

    def initialize(basic_block, operation, operands=[], name=nil)
      super(basic_block, operands, name)
      self.operation = operation
    end

    def operation=(operation)
      unless OPERATIONS.include? operation
        raise ArgumentError, "invalid integer operation #{operation}"
      end

      @operation = operation
    end

    def pretty_parameters(p=LIR::PrettyPrinter.new)
      p.keyword @operation
    end

    def type
      case @operation
      when :+, :-, :*, :/, :%
        Type.klass(VI::Integer)
      when :<, :<=, :>, :>=, :==, :!=
        Type.klass(VI::Object)
      else
        Type.top
      end
    end
  end
end

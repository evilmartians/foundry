module Foundry
  class LIR::IntegerOpInsn < Furnace::SSA::Instruction
    OPERATIONS = [ :+, :-, :*, :/, :%, :<, :<=, :>, :>=, :==, :!= ]

    attr_accessor :operation

    syntax do |s|
      s.operand :left
      s.operand :right
    end

    def initialize(operation, operands=[], name=nil)
      super(operands, name)

      self.operation = operation
    end

    def operation=(operation)
      unless OPERATIONS.include? operation
        raise ArgumentError, "invalid integer operation #{operation}"
      end

      @operation = operation
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.keyword @operation
    end

    def type
      if @operation == :* && left.type != right.type
        #p "INTOP", left, right, left.type, right.type
        #require 'pry';binding.pry
      end

      if left.type == right.type
        case @operation
        when :+, :-, :*, :/, :%
          left.type
        when :<, :<=, :>, :>=, :==, :!=
          Type.boolean
        end
      else
        Type.top
      end
    end
  end
end

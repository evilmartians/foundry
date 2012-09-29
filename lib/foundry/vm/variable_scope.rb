module Foundry
  class VariableScope < VMImmediate
    attr_reader :self
    attr_reader :module
    attr_reader :parent
    attr_reader :const_scope
    attr_reader :arguments
    attr_reader :block

    attr_reader :locals

    attr_accessor :eval_scope

    attr_accessor :line, :function

    def initialize(ourself, modulus, parent, const_scope, arguments, block)
      @self, @module, @parent, @const_scope = ourself, modulus, parent, const_scope
      @arguments, @block = arguments.to_a.dup.freeze, block
      @eval_scope = false
      @locals = ::Hash.new { VI::NIL }

      @line     = 0
      @function = '(unknown)'
    end
  end
end
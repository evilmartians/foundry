module Foundry
  class Executable < VMImmediate
    attr_reader :ast

    def initialize(ast)
      @ast = ast
    end

    def execute(scope)
      interp = Interpreter.new(self, scope)
      interp.evaluate
    end
  end
end
module Foundry
  class Executable < VMImmediate
    attr_reader :ast, :file

    def initialize(ast, file)
      @ast  = ast
      @file = file
    end

    def execute(outer, scope)
      interp = Interpreter.new(outer, self, scope)
      interp.evaluate
    end
  end
end
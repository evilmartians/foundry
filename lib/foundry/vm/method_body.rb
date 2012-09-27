module Foundry
  class MethodBody < Executable
    attr_reader :module
    attr_reader :parameters

    def initialize(ast, modulus, parameters, primitive)
      super(ast)
      @module     = modulus
      @parameters = parameters
      @primitive  = primitive
    end

    def execute(scope)
      if @primitive
        if value = @module.__send__(:"_#{@primitive}", scope)
          return value
        end
      end

      super
    end
  end
end
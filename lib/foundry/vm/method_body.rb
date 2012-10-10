module Foundry
  class MethodBody < Executable
    attr_reader :module
    attr_reader :parameters

    def initialize(ast, file, modulus, parameters)
      super(ast, file)
      @module     = modulus
      @parameters = parameters
    end
  end
end
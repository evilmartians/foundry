module Foundry
  class VMProc < VMObject
    attr_reader :binding, :code

    def initialize(klass, binding, code)
      super(klass)

      @binding = binding
      @code    = code
    end

    def call(self_, arguments, block, outer)
      Runtime.interpreter.
        new(@code, self_, arguments, block, @binding, outer).
        evaluate
    end

    def inspect
      "{Proc}"
    end
  end
end
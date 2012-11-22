module Foundry
  class VMProc < VMObject
    attr_reader :binding, :code

    def initialize(klass, binding, code)
      super(klass)

      @binding = binding
      @code    = code
    end

    def call(receiver, arguments, block, outer)
      Runtime.interpreter.
        new(@code, receiver, arguments, block, @binding, outer).
        evaluate
    end

    def inspect
      "{Proc}"
    end
  end
end
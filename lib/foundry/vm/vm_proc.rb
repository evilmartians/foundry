module Foundry
  class VMProc < VMImmediate
    attr_reader :code, :binding

    define_mapped_ivars :binding

    def initialize(code, binding)
      @code    = code
      @binding = binding
    end

    def class
      VI::Proc
    end

    def call(self_, arguments, block, outer)
      Runtime.interpreter.
        new(self, self_, arguments, block, outer).
        evaluate
    end

    def inspect
      "{Proc}"
    end
  end
end

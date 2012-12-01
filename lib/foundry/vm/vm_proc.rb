module Foundry
  class VMProc < VMObject
    attr_reader :binding, :lambda, :code

    define_mapped_ivars :binding, :lambda

    def vm_initialize(binding, code)
      @binding = binding
      @lambda  = VI::FALSE
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
module Foundry
  class VMProc < VMObject
    attr_reader :binding, :code

    define_mapped_ivars :binding

    def vm_initialize(binding, code)
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
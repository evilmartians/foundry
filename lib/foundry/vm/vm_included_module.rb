module Foundry
  class VMIncludedModule < VMModule
    attr_reader :module
    attr_reader :upperclass

    def vm_initialize(modulus, upperclass)
      @module       = modulus
      @name         = modulus.name
      @upperclass   = upperclass
      @const_table  = modulus.const_table
      @method_table = modulus.method_table
    end

    def ancestors
      [ @module ] + @upperclass.ancestors
    end

    def inspect
      "{IncludedModule #{@name}}"
    end
  end
end
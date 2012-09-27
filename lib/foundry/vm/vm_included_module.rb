module Foundry
  class VMIncludedModule < VMModule
    attr_reader :module
    attr_reader :upperclass

    def initialize(klass, module_, upperclass)
      super(klass)

      @module       = module_
      @upperclass   = upperclass
      @const_table  = module_.const_table
      @method_table = module_.method_table
    end

    def ancestors
      [ @module ] + @upperclass.ancestors
    end

    def inspect
      "{IncludedModule #{@name}}"
    end
  end
end
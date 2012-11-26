module Foundry
  class VMIncludedModule < VMModule
    attr_reader :module

    define_mapped_ivars :module

    def vm_initialize(module_, superclass)
      @module         = module_
      @superclass     = superclass

      @constant_table = module_.constant_table
      @method_table   = module_.method_table
    end

    def name
      @module.name
    end

    def ancestors
      [ @module ] + @superclass.ancestors
    end

    def inspect
      "{IncludedModule #{name}}"
    end
  end
end
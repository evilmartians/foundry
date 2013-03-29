class Foundry::IncludedModule < Module
  def initialize(mod, superclass)
    @module         = mod
    @superclass     = superclass

    @constant_table = mod.constant_table
    @method_table   = mod.method_table
  end

  def module
    @module
  end

  def name
    @module.name
  end

  def to_s
    @module.to_s
  end
end

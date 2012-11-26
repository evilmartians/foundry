class Module
  def name
    @name
  end

  def superclass=(superclass)
    @superclass = superclass
  end

  def direct_superclass
    @superclass
  end

  def constant_table
    @constant_table
  end

  def method_table
    @method_table
  end

  def include(mod)
    mod.append_features(self)
    mod.included(self)
  end

  def include_into(mod)
    mod.superclass = Foundry::IncludedModule.new(self, mod.direct_superclass)
  end

  alias append_features include_into

  def included(mod)
  end

  def public(*)
    # TODO
  end

  def private(*)
    # TODO
  end

  def protected(*)
    # TODO
  end

  private :public, :private, :protected
end
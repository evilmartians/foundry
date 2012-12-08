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

  def const_get(Symbol name, search_parent=true)
    if @constant_table.key? name
      @constant_table[name]
    elsif search_parent && @superclass
      @superclass.const_get(name, search_parent)
    else
      raise NameError, "uninitialized constant #{name} for #{self}"
    end
  end

  def const_set(Symbol name, value)
    if @constant_table.key? name
      raise NameError, "already initialized constant #{name} for #{self}"
    else
      @constant_table[name] = value
    end
  end

  def constants(search_parent=true)
    list = @constant_table.keys

    if search_parent && @superclass
      list |= @superclass.constants
    end

    list
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
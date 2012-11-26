class Class < Module
  def allocate
    FoundryRt.allocate self
  end

  def new(*args)
    instance = allocate
    instance.initialize(*args)
    instance
  end

  def superclass
    klass = @superclass

    while klass.is_a? Foundry::IncludedModule
      klass = klass.direct_superclass
    end

    klass
  end
end
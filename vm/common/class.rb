class Class < Module
  def coerce(object)
    if FoundryRt.is_a? object, self
      object
    else
      raise TypeError, "coerce: #{object.class} is not a #{self}"
    end
  end

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

  def ancestors
    klass     = self
    ancestors = []

    until klass.nil?
      ancestors += [ klass ]
      klass = klass.superclass
    end

    ancestors
  end
end

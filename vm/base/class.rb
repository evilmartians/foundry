class Class < Module
  def allocate
    Foundry.primitive :allocate
  end

  def new(*args)
    instance = allocate
    instance.initialize(*args)
    instance
  end
end
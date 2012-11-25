class Class < Module
  def allocate
    FoundryRt.allocate self
  end

  def new(*args)
    instance = allocate
    instance.initialize(*args)
    instance
  end
end
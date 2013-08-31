class Value
  def self.new(*args, **kwargs)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args, **kwargs)
  end

  def initialize
  end

  def ==(other)
    invokeprimitive obj_equal(self, other)
  end
end

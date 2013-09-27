class Array
  def self.new(reserve: 0)
    invokeprimitive ary_alloc(\element, reserve)
  end

  def [](index) : (Array(\element), Unsigned(32)) -> \element
    invokeprimitive ary_get(self, index)
  end

  def []=(index, value) : (Array(\element), Unsigned(32), \element) -> Nil
    invokeprimitive ary_set(self, index, value)
  end

  def each(block)
    let mut index = 0
    let     count = invokeprimitive ary_capa(self)

    while index < count
      block.call(self[index])
      index += 1
    end

    self
  end
end

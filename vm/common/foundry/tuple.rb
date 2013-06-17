class Foundry::Tuple
  def size
    FoundryRt.tuple_size(self)
  end

  alias length size

  def [](index)
    FoundryRt.tuple_lookup(self, index)
  end

  def +(other)
    FoundryRt.tuple_concat(self, other)
  end

  def any?
    length > 0
  end

  def each
    index = 0

    while index < length
      yield self[index]
      index += 1
    end

    self
  end

  def include?(elem)
    index = 0
    found = false

    while index < length
      if elem == self[index]
        found = true
      end

      index += 1
    end

    found
  end
end

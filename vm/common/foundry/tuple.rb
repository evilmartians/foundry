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

  def each
    i = 0

    while i < length
      yield self[i]
      i += 1
    end

    self
  end
end

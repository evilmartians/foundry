module Foundry::LookupTable
  def each(&block)
    FoundryRt.lut_traverse(self, block)
  end

  def [](key)
    FoundryRt.lut_lookup(self, key)
  end

  def []=(key, value)
    FoundryRt.lut_store(self, key, value)
  end

  def key?(key)
    FoundryRt.lut_check(self, key)
  end

  def keys
    keys = []

    each do |key,|
      keys << key
    end

    key
  end
end
class Unit < Value
  def @base : Unsigned(32)

  def initialize(base)
    @base = base
  end

  def self.register(name, kind, offset:, align:, impl:)
    if kind == :r || kind == :rw
      self.define_method(name, (self) do
        impl.new(invokeprimitive mem_loadv(align, @base + offset))
      end)
    end

    if kind == :w || kind == :rw
      self.define_method(:"#{name}=", (self, reg) do
        invokeprimitive mem_storev(align, @base + offset, reg.value)
      end)
    end
  end
end

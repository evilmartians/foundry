class Unit < Value
  def @base : Unsigned(32)

  def initialize(base)
    @base = base
  end

  def self.register(name, kind, offset:, align:, impl:,
                    invariant_set: 0, invariant_clear: 0)
    if kind == :r || kind == :rw || kind == :rwc
      self.define_method(name, (self) do
        impl.new(invokeprimitive mem_loadv(align, @base + offset))
      end)
    end

    if kind == :w || kind == :rw || kind == :rwc
      self.define_method(:"#{name}=", (self, reg) do
        invokeprimitive mem_storev(align, @base + offset, reg.value)
      end)
    end

    if kind == :rwc
      self.define_method(:"clear_#{name}", (self) do
        let value = invokeprimitive mem_loadv(align, @base + offset)
        impl.new(value | invariant_set & ~invariant_clear)
      end)
    end
  end

  def self.registers(name, kind, offset:, spacing: 0, count:, align:, impl:,
                     invariant_set: 0, invariant_clear: 0)
    if kind == :r || kind == :rw || kind == :rwc
      self.define_method(name, (self, n) do
        let addr = @base + offset + n * (align + spacing)
        impl.new(invokeprimitive mem_loadv(align, addr))
      end)
    end

    if kind == :w || kind == :rw || kind == :rwc
      self.define_method(:"#{name}=", (self, n, reg) do
        let addr = @base + offset + n * (align + spacing)
        invokeprimitive mem_storev(align, addr, reg.value)
      end)
    end

    if kind == :rwc
      self.define_method(:"clear_#{name}", (self, n) do
        let addr  = @base + offset + n * (align + spacing)
        let value = invokeprimitive mem_loadv(align, addr)
        impl.new(value | invariant_set & ~invariant_clear)
      end)
    end
  end
end

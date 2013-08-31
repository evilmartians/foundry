class Register(\width) < Value
  def @value : Unsigned(\width)

  def initialize(value)
    @value = value
  end

  def value
    @value
  end

  def self.flag(name, kind, offset:)
    let mask = 1u32 << offset

    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        @value & mask != 0u32
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, field_value) do
        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end
  end

  def self.flags(name, kind, offset:, spacing:)
    if kind == :r or kind == :rw
      self.define_method(name, (self, n) do
        let mask = 1u32 << (offset + n * (1u32 + spacing))

        @value & mask != 0u32
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, n, field_value) do
        let mask = 1u32 << (offset + n * (1u32 + spacing))

        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end
  end

  def self.field(name, kind, offset:, width:)
    let mask = (1u32 << width) - 1u32

    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        (@value >> offset) & mask
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, field_value) do
        self do
          @value = @value & ~(mask << offset) | ((field_value & mask) << offset)
        end
      end)
    end
  end

  def self.fields(name, kind, offset:, width:, spacing:)
    let mask = (1u32 << width) - 1u32

    if kind == :r or kind == :rw
      self.define_method(name, (self, n) do
        let field_offset = offset + n * (width + spacing)

        (@value >> field_offset) & mask
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, n, field_value) do
        let field_offset = offset + n * (width + spacing)

        self do
          @value = @value & ~(mask << field_offset) |
                            ((field_value & mask) << field_offset)
        end
      end)
    end
  end
end

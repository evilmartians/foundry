class Register(\width) < Value
  def @value : Unsigned(\width)

  def initialize(value)
    @value = value
  end

  def value
    @value
  end

  def self.flag(name, kind, offset:)
    let mask = 1 << offset

    if kind == :r || kind == :rw || kind == :rc_w0 || kind == :rc_w1 || kind == :t
      self.define_method(name, (self) do
        @value & mask != 0
      end)
    end

    if kind == :w || kind == :rw
      self.define_method(:"set_#{name}", (self, field_value) do
        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end

    if kind == :rc_w0
      self.define_method(:"clear_#{name}", (self) do
        self { @value = @value & ~mask }
      end)
    end

    if kind == :rc_w1 || kind == :t
      let meth = (if    kind == :rc_w1 then :"clear_#{name}"
                  elsif kind == :t     then :"toggle_#{name}"
                  end)

      self.define_method(meth, (self) do
        self { @value = @value | mask }
      end)
    end
  end

  def self.flags(name, kind, offset:, spacing: 0, count:)
    if kind == :r || kind == :rw || kind == :rc_w0 || kind == :rc_w1 || kind == :t
      self.define_method(name, (self, n) do
        let mask = 1 << (offset + n * (1 + spacing))

        @value & mask != 0
      end)
    end

    if kind == :w || kind == :rw
      self.define_method(:"set_#{name}", (self, n, field_value) do
        let mask = 1 << (offset + n * (1 + spacing))

        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end

    if kind == :rc_w0
      self.define_method(:"clear_#{name}", (self, n) do
        let mask = 1 << (offset + n * (1 + spacing))

        self { @value = @value & ~mask }
      end)
    end

    if kind == :rc_w1 || kind == :t
      let meth = (if    kind == :rc_w1 then :"clear_#{name}"
                  elsif kind == :t     then :"toggle_#{name}"
                  end)

      self.define_method(meth, (self, n) do
        let mask = 1 << (offset + n * (1 + spacing))

        self { @value = @value | mask }
      end)
    end
  end

  def self.field(name, kind, offset:, width:)
    let mask = (1 << width) - 1

    if kind == :r || kind == :rw || kind == :t
      self.define_method(name, (self) do
        (@value >> offset) & mask
      end)
    end

    if kind == :w || kind == :rw || kind == :t
      let meth = (if kind == :t then :"toggle_#{name}"
                  else               :"set_#{name}"
                  end)

      self.define_method(meth, (self, field_value) do
        self do
          @value = @value & ~(mask << offset) | ((field_value & mask) << offset)
        end
      end)
    end
  end

  def self.fields(name, kind, offset:, spacing: 0, count:, width:)
    let mask = (1 << width) - 1

    if kind == :r || kind == :rw
      self.define_method(name, (self, n) do
        let field_offset = offset + n * (width + spacing)

        (@value >> field_offset) & mask
      end)
    end

    if kind == :w || kind == :rw
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

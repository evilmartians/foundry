module Machine
  class Integer < Integer
    def +(other)
      FoundryRt.intop :+, self, other
    end

    def -(other)
      FoundryRt.intop :-, self, other
    end

    def *(other)
      FoundryRt.intop :*, self, other
    end

    def /(other)
      FoundryRt.intop :/, self, other
    end

    def %(other)
      FoundryRt.intop :%, self, other
    end

    def ==(other)
      FoundryRt.intop :==, self, other
    end

    def !=(other)
      FoundryRt.intop :!=, self, other
    end

    def <(other)
      FoundryRt.intop :<,  self, other
    end

    def <=(other)
      FoundryRt.intop :<=, self, other
    end

    def >(other)
      FoundryRt.intop :>,  self, other
    end

    def >=(other)
      FoundryRt.intop :>=, self, other
    end

    def to_s
      FoundryRt.intop :to_s, self
    end

    def to_unsigned
      FoundryRt.int_convert self, false, @@width
    end

    def to_signed
      FoundryRt.int_convert self, true,  @@width
    end
  end

  U8  = Integer.reify(width: 8,  signed: false)
  U16 = Integer.reify(width: 16, signed: false)
  U32 = Integer.reify(width: 32, signed: false)
  U64 = Integer.reify(width: 64, signed: false)

  S8  = Integer.reify(width: 8,  signed: true)
  S16 = Integer.reify(width: 16, signed: true)
  S32 = Integer.reify(width: 32, signed: true)
  S64 = Integer.reify(width: 64, signed: true)
end

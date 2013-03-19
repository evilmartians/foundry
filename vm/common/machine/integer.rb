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

    def to_u8
      FoundryRt.int_convert self, false, 8
    end

    def to_u16
      FoundryRt.int_convert self, false, 16
    end

    def to_u32
      FoundryRt.int_convert self, false, 32
    end

    def to_u64
      FoundryRt.int_convert self, false, 64
    end

    def to_s8
      FoundryRt.int_convert self, true, 8
    end

    def to_s16
      FoundryRt.int_convert self, true, 16
    end

    def to_s32
      FoundryRt.int_convert self, true, 32
    end

    def to_s64
      FoundryRt.int_convert self, true, 64
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

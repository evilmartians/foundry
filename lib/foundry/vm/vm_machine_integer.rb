module Foundry
  class VMMachineInteger < VMObject
    attr_reader :value

    def vm_initialize(value)
      @width  = @class.specializations[VMSymbol.new(:width)].to_int
      @signed = (@class.specializations[VMSymbol.new(:signed)] == VI::TRUE)

      # Emulate 2's complement wraparound.

      # For 16 bits:
      #         value & 0xFFFF
      value   = value & ((1 << @width) - 1)

      if signed?
        # For 16 bits:
        #  value & 0x8000
        if value & (1 << @width - 1) != 0
          #        value - 0x10000
          @value = value - (1 << @width)
        else
          #        value & 0x7FFF
          @value = value & ((1 << @width - 1) - 1)
        end
      else
        @value = value
      end
    end

    def width
      @width
    end

    def signed?
      @signed
    end

    alias to_int value
    alias to_i   value

    def hash
      @value.hash
    end

    def eql?(other)
      other.respond_to?(:to_int) &&
          @value == other.to_int
    end

    def inspect
      if @signed
        type = "S#{@width}"
      else
        type = "U#{@width}"
      end

      "{#{type} #{@value}}"
    end
  end
end

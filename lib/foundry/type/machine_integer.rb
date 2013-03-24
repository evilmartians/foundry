module Foundry
  class Type::MachineInteger < Type::Top
    attr_reader :signed, :width

    def initialize(signed, width)
      @signed, @width = signed.to_type, width.to_type
    end

    def replace_type_with(type, replacement)
      new_signed = (@signed == type) ? replacement : @signed
      new_width  = (@width  == type) ? replacement : @width

      Type::MachineInteger.new(new_signed, new_width)
    end

    def reified?
      !@signed.variable? &&
          !@width.variable?
    end

    def hash
      [Type::MachineInteger, @signed, @width].hash
    end

    def eql?(other)
      other.is_a?(Type::MachineInteger) &&
          other.signed == signed &&
          other.width  == width
    end

    def to_klass
      unless reified?
        raise "Cannot convert non-reified MachineInteger to klass"
      end

      VI::Machine_Integer.reify(
          signed: @signed.value,
          width:  @width.value)
    end

    def to_s
      Furnace::AwesomePrinter.new(false) do |p|
        awesome_print(p)
      end
    end

    def awesome_print(p=Furnace::AwesomePrinter.new)
      if reified? && @width.value != VI::NIL
        width = @width.value.to_int

        if @signed.value == VI::TRUE
          p.type "s#{width}"
        else
          p.type "u#{width}"
        end
      else
        p.type 'machine_integer'

        p <<   '<'
        p.text 'signed:'
        @signed.awesome_print p
        p.text 'width:'
        @width.awesome_print  p
        p.text '>'
      end
    end
  end
end

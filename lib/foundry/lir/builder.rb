module Foundry
  class LIR::Builder < Furnace::SSA::Builder
    def self.scope
      LIR
    end

    def constant(value)
      LIR::Constant.new(value.type, value)
    end

    def nil
      constant VI::NIL
    end

    def false
      constant VI::FALSE
    end

    def true
      constant VI::TRUE
    end

    def integer(value)
      constant VI.new_integer(value)
    end

    def symbol(value)
      constant VI.new_symbol(value)
    end

    def string(value)
      constant VI.new_string(value)
    end

    def const_base
      constant VI::Object
    end

    def toplevel
      constant VI::TOPLEVEL
    end

    def tuple(elements=[])
      if elements.all? &:constant?
        constant VI.new_tuple(elements.map(&:value))
      else
        super
      end
    end
  end
end
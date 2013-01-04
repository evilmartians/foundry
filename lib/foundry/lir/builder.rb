module Foundry
  class LIR::Builder < Furnace::SSA::Builder
    def self.scope
      LIR
    end

    def nil
      LIR::Constant.new(VI::NilClass, VI::NIL)
    end

    def false
      LIR::Constant.new(VI::FalseClass, VI::FALSE)
    end

    def true
      LIR::Constant.new(VI::TrueClass, VI::TRUE)
    end

    def integer(value)
      LIR::Constant.new(VI::Integer, VI.new_integer(value))
    end

    def symbol(value)
      LIR::Constant.new(VI::Symbol, VI.new_symbol(value))
    end

    def string(value)
      LIR::Constant.new(VI::String, VI.new_string(value))
    end

    def const_base
      LIR::Constant.new(VI::Class, VI::Object)
    end

    def toplevel
      LIR::Constant.new(VI::Object, VI::TOPLEVEL)
    end
  end
end
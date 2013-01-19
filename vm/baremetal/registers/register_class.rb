module Registers
  class RegisterClass < Object.reify(scalar: true)
    parametric_by :@@type, :@@address, :@@alignment, :@@combine

    def initialize
      if @@combine
        @value = load

        yield self

        store @value
      end
    end

    def value
      if @@combine
        @value
      else
        load
      end
    end

    def value=(new_value)
      if @@combine
        @value = new_value
      else
        store new_value
      end
    end

    def fetch(Foundry::Tuple fields)
      combining do |reg|
        fields.map do |field|
          reg.send field
        end
      end
    end

    def update(Foundry::LookupTable fields)
      combining do |reg|
        fields.each do |field, value|
          reg.send :"#{field}=", value
        end

        reg.store
      end
    end

    def combining(&block)
      self.class.reify(combine: true).new(&block)
    end

    protected

    def load
      FoundryRt.read_atomic(@@type, @@address, @@alignment, :seq_cst)
    end

    def store(new_value)
      FoundryRt.write_atomic(@@type, @@address, @@alignment, new_value, :seq_cst)
    end
  end
end
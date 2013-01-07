module Foundry
  class ClosureType < LIR::Type
    def subtype_of?(other)
      super ||
          other.instance_of?(Monotype) &&
          other.klass == VI::Proc
    end

    def inspect
      "closure"
    end
  end
end
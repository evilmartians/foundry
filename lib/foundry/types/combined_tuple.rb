module Foundry
  class CombinedTupleType < TupleType
    attr_accessor :fragments

    def initialize(fragments=nil)
      @fragments = fragments
    end

    def element_types
      if @fragments && @fragments.all?
        @fragments.map do |elem|
          elem.element_types
        end.reduce(:+)
      end
    end
  end
end
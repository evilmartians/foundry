module Foundry
  class HIR::Context
    attr_accessor :root

    def initialize(root)
      @root = root
    end
  end
end
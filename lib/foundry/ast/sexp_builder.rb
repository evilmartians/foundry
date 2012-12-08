module Foundry
  module AST::SexpBuilder
    def s(type, *children)
      AST::Node.new(type, children)
    end
  end
end
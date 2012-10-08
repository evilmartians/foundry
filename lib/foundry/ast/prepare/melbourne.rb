module Foundry::AST
  module Prepare
    class Melbourne < Transform
      def on_scope(node)
        block, = node.children
        visit(block)
      end
    end
  end
end
module Foundry::AST
  module Prepare
    class Melbourne < Transform
      def on_scope(node)
        block, = node.children
        if block
          visit(block)
        else
          node.update(:block)
        end
      end

      def on_lit(node)
        value, = node.children

        case value
        when Integer
          node.update(:lit_integer, [ value ])
        when Symbol
          node.update(:lit_symbol,  [ value ])
        else
          node
        end
      end

      def on_str(node)
        node.update(:lit_string)
      end
    end
  end
end
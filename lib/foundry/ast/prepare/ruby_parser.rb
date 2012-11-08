module Foundry
  module AST::Prepare
    class RubyParser < AST::Transform
      def on_class(node)
        name, superclass, *code = node.children
        node.updated(:class, [
          name, process(superclass),
          process(node.updated(:block, code))
        ])
      end

      def on_defn(node)
        name, args, *code = node.children
        node.updated(:defn, [
          name, process(args),
          process(node.updated(:block, code))
        ])
      end

      def on_call(node)
        receiver, name, *args = node.children
        node.updated(:call, [
          process(receiver), name,
          process(node.updated(:array, args))
        ])
      end

      def on_lit(node)
        value, = node.children

        case value
        when Integer
          node.updated(:integer)
        when Symbol
          node.updated(:symbol)
        when Float
          node.updated(:float)
        else
          node
        end
      end

      def on_str(node)
        node.updated(:string)
      end
    end
  end
end
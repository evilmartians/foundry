module Foundry
  module AST::Prepare
    class RubyParser < AST::Transform
      def process_const_name(name)
        if name.is_a? Symbol
          AST::Node.new(:const_lookup, [ name ])
        else
          name
        end
      end

      def on_module(node)
        name, *code = node.children
        node.updated(nil, [
          process_const_name(name),
          process(node.updated(:block, code))
        ])
      end

      def on_class(node)
        name, superclass, *code = node.children

        node.updated(nil, [
          process_const_name(name), process(superclass),
          process(node.updated(:block, code))
        ])
      end

      def on_const(node)
        node.updated(:const_lookup)
      end

      def on_colon2(node)
        node.updated(:const_access)
      end

      def on_colon3(node)
        node.updated(:const_toplevel)
      end

      def on_cdecl(node)
        name, value = node.children
        node.updated(:const_declare, [
          process_const_name(name),
          process(value)
        ])
      end

      def on_defn(node)
        name, args, *code = node.children
        node.updated(nil, [
          name, process(args),
          process(node.updated(:block, code))
        ])
      end

      def on_call(node)
        receiver, name, *args = node.children
        node.updated(nil, [
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
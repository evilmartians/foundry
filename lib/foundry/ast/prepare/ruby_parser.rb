module Foundry
  module AST::Prepare
    class RubyParser < AST::Processor
      def initialize(is_eval)
        @is_eval = is_eval
      end

      def transform(ast)
        if ast.type == :block
          code = ast.children
        else
          code = [ ast ]
        end

        if @is_eval
          ast.updated(:eval_block,
            process_all(code))
        else
          ast.updated(:toplevel_block,
            process_all(code))
        end
      end

      def process_const_name(name)
        if name.is_a? Symbol
          Node(:const_ref, [ name ])
        else
          process(name)
        end
      end

      def on_module(node)
        name, *code = node.children
        node.updated(nil, [
          process_const_name(name),
          *process_all(code)
        ])
      end

      def on_class(node)
        name, superclass, *code = node.children

        node.updated(nil, [
          process_const_name(name), process(superclass),
          *process_all(code)
        ])
      end

      def on_const(node)
        node.updated(:const_ref)
      end

      def on_colon2(node)
        scope, name = node.children

        node.updated(:const_fetch, [
          process(scope), name
        ])
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
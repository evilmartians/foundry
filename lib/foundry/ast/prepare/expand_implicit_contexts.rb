module Foundry
  module AST::Prepare
    class ExpandImplicitContexts < AST::Processor
      def on_toplevel_block(node)
        node.updated(:let, [
          {
            :Self  => s(:self),
            :Block => s(:nil),
            :Defn  => s(:const_base),
            :Cref  => s(:array),
          },
          *process_all(node.children),
        ], function: '(toplevel)')
      end

      def on_eval_block(node)
        node.updated(:block,
          process_all(node.children),
          function: '(eval)')
      end

      def process_scope(node, new_scope, body, function)
        node.updated(:let, [
          {
            :Scope  => new_scope,

            :Self   => s(:var, :Scope),
            :Block  => s(:nil),
            :Defn   => s(:var, :Scope),
            :Cref   => s(:array_unshift,
                         s(:var, :Cref),
                         s(:var, :Scope)),
          },
          *process_all(body)
        ], function: function)
      end

      def on_class(node)
        scope, name, superclass, *body = node.children

        process_scope(node,
          node.updated(:define_class, [
            process(scope), name, process(superclass)
          ]),
          body,
          '<class body>')
      end

      def on_sclass(node)
        scope, *body = node.children

        process_scope(node,
          node.updated(:singleton_class_of, [
            process(scope)
          ]),
          body,
          '<singleton class body>')
      end

      def on_module(node)
        scope, name, *body = node.children

        process_scope(node,
          node.updated(:define_module, [
            process(scope), name
          ]),
          body,
          '<module body>')
      end

      def on_const_ref(node)
        name, = node.children

        node.updated(:const_ref_in, [
          s(:var, :Cref),
          name
        ])
      end

      def on_const_toplevel(node)
        name, = node.children

        node.updated(:const_fetch, [
          s(:const_base),
          name
        ])
      end

      def on_defn(node)
        name, args, *code = node.children

        node.updated(:def, [
          s(:var, :Defn),
          name, args,
          *process_all(code)
        ])
      end

      def on_defs(node)
        target, name, args, *code = node.children

        node.updated(:def, [
          s(:singleton_class_of, process(target)),
          name, args,
          *process_all(code)
        ])
      end

      def on_call(node)
        receiver, name, args, block = node.children

        node.updated(nil, [
          process(receiver), name,
          process(args),
          process(block)
        ])
      end

      def on_self(node)
        node.updated(:var, [ :Self ])
      end

      def on_yield(node)
        node.updated(:proc_call, [
          s(:check_block, s(:var, :Block)),
          node.updated(:array),
          s(:nil)
        ])
      end

      def on_ivar(node)
        name, = node.children

        node.updated(nil, [
          s(:var, :Self),
          name
        ])
      end

      def on_iasgn(node)
        name, value = node.children

        node.updated(nil, [
          s(:var, :Self),
          name, process(value)
        ])
      end
    end
  end
end
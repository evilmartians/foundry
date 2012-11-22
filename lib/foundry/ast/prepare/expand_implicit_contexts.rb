module Foundry
  module AST::Prepare
    class ExpandImplicitContexts < AST::Processor
      def on_toplevel_block(node)
        node.updated(:let_new, [
          {
            :Self  => Node(:self),
            :Block => Node(:nil),
            :Defn  => Node(:const_base),
            :Cref  => Node(:array),
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
        node.updated(:let_new, [
          {
            # Intermediate
            :Scope  => new_scope,

            :Self   => Node(:var, [ :Scope ]),
            :Block  => Node(:nil),
            :Defn   => Node(:var, [ :Scope ]),
            :Cref   => Node(:array_unshift, [
                         Node(:var, [ :Cref  ]),
                         Node(:var, [ :Scope ])
                       ]),
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
          '<class definition>')
      end

      def on_module(node)
        scope, name, *body = node.children

        process_scope(node,
          node.updated(:define_module, [
            process(scope), name
          ]),
          body,
          '<module definition>')
      end

      def on_const_ref(node)
        name, = node.children

        node.updated(:const_ref_in, [
          Node(:var, [ :Cref ]),
          name
        ])
      end

      def on_const_toplevel(node)
        name, = node.children

        node.updated(:const_fetch, [
          Node(:const_base),
          name
        ])
      end

      def on_defn(node)
        name, args, *code = node.children

        node.updated(:def, [
          Node(:var, [ :Defn ]),
          name, args,
          *process_all(code)
        ])
      end

      def on_defs(node)
        target, name, args, *code = node.children

        node.updated(:def, [
          Node(:singleton_class_of, [
            target
          ]),
          name, args,
          *process_all(code)
        ])
      end

      def on_call(node)
        receiver, name, args = node.children

        if receiver.nil?
          receiver = Node(:var, [ :Self ])
        end

        node.updated(nil, [
          process(receiver), name,
          process(args)
        ])
      end

      def on_self(node)
        node.updated(:var, [ :Self ])
      end
    end
  end
end
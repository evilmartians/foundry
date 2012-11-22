module Foundry
  module AST::Prepare
    class ExpandImplicitContexts < AST::Processor
      def on_toplevel_block(node)
        node.updated(:let_new, [
          {
            :Self  => Node(:self),
            :Defn  => Node(:const_base),
            :Cref  => Node(:array),
            :Block => Node(:nil),
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
            :Defn   => Node(:var, [ :Scope ]),
            :Cref   => Node(:array_unshift, [
                         Node(:var, [ :Cref  ]),
                         Node(:var, [ :Scope ])
                       ]),
            :Block  => Node(:nil),
          },
          *process_all(body)
        ], function: function)
      end

      def on_class(node)
        name, superclass, *body = node.children

        process_scope(node,
          node.updated(:define_class, [ name, superclass ]),
          body,
          '<class definition>')
      end

      def on_module(node)
        name, *body = node.children

        process_scope(node,
          node.updated(:define_module, [ name ]),
          body,
          '<module definition>')
      end

      def on_const_toplevel(node)
        name, = node.children

        node.updated(:const_fetch, [
          node.updated(:const_base),
          name
        ])
      end

      def on_self(node)
        node.updated(:var, [ :Self ])
      end
    end
  end
end
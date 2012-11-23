module Foundry
  module AST::Prepare
    class TraceVariables < AST::Processor
      def initialize(locals=nil)
        @locals = locals || []
      end

      def transform(root)
        @static_env = Set.new(@locals)
        @let_vars   = nil

        process root
      end

      def on_let(node)
        upper_static_env = @static_env
        upper_let_vars   = @let_vars

        vars, *body = node.children

        @let_vars   = vars.dup
        @static_env = @static_env.merge(@let_vars.keys)

        node.updated(nil, [
          @let_vars, *process_all(body)
        ])

      ensure
        @static_env = upper_static_env
        @let_vars   = upper_let_vars
      end

      def on_lasgn(node)
        name, value = node.children

        unless @static_env.include? name
          @static_env.add name

          if @let_vars
            @let_vars[name] = s(:nil)
          else
            # TODO: document this eval-related hack
            return node.updated(:eval_mut!, [
              name, process(value)
            ])
          end
        end

        node.updated(:mut!, [
          name, process(value)
        ])
      end

      def on_lvar(node)
        node.updated(:var)
      end
    end
  end
end
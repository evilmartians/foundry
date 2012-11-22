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

      def process_let(node, flush_env)
        upper_static_env = @static_env
        upper_let_vars   = @let_vars

        vars, *body = node.children

        @let_vars = vars

        if flush_env
          @static_env = Set.new(vars.keys)
        else
          @static_env = upper_static_env.merge(vars.keys)
        end

        node.updated(nil, [
          vars, *process_all(body)
        ])
      ensure
        @static_env = upper_static_env
        @let_vars   = upper_let_vars
      end

      def on_lasgn(node)
        name, = node.children

        unless @static_env.include? name
          @static_env.add name

          if @let_vars
            @let_vars[name] = Node(:nil)
          else
            # TODO: document this eval-related hack
            return node.updated(:eval_mut!)
          end
        end

        node.updated(:mut!)
      end

      def on_lvar(node)
        node.updated(:var)
      end
    end
  end
end
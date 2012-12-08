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

      def process_const_name(what)
        if what.is_a? Symbol
          scope =
            s(:array_ref,
              s(:var, :Cref), 0)
          name  = what

        elsif what.type == :colon2
          scope, name = what.children
          scope = process(scope)

        elsif what.type == :colon3
          name, = what.children
          scope =
            s(:const_base)

        end

        [ scope, name ]
      end

      def on_module(node)
        name, *code = node.children
        node.updated(nil, [
          *process_const_name(name),
          *process_all(code)
        ])
      end

      def on_class(node)
        name, superclass, *code = node.children

        superclass = s(:nil) if superclass.nil?

        node.updated(nil, [
          *process_const_name(name),
           process(superclass),
          *process_all(code)
        ])
      end

      def on_sclass(node)
        scope, *code = node.children

        node.updated(nil, [
           process(scope),
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
          *process_const_name(name),
           process(value)
        ])
      end

      def on_defn(node)
        name, args, *code = node.children
        node.updated(nil, [
           s(:symbol, name), process(args),
          *process_all(code)
        ])
      end

      def on_defs(node)
        scope, name, args, *code = node.children
        node.updated(nil, [
           process(scope),
           s(:symbol, name), process(args),
          *process_all(code)
        ])
      end

      def on_iter(node)
        call, block_args, *block_body = node.children

        block = node.updated(:proc, [
          block_args, *block_body
        ])

        receiver, name, args = process(call).children

        call.updated(nil, [
          receiver, name, args,
          process(block)
        ])
      end

      def on_call(node)
        receiver, name, *args = node.children

        if receiver.nil?
          receiver = s(:self)
        end

        if args.length > 0 &&
             args.last.type == :block_pass
          block, = args.last.children
          args   = args[0..-2]
        else
          block  = s(:nil)
        end

        node.updated(nil, [
          process(receiver), name,
          process(node.updated(:array, args)),
          process(block)
        ])
      end

      def on_yield(node)
        node.updated(nil, process_all(node.children))
      end

      def on_tvar(node)
        klass, declaration = node.children
        node.updated(:typed_arg, [
          process(klass), on_arg(declaration)
        ])
      end

      def on_rtype(node)
        type, = node.children
        node.updated(:returns, [
          process(type)
        ])
      end

      # Not present in RP output.
      def on_arg(arg)
        if arg.is_a? Symbol
          if arg == :*
            s(:splat_arg)
          elsif arg[0] == '*'
            s(:splat_arg, arg[1..-1].to_sym)
          elsif arg[0] == '&'
            s(:block_arg, arg[1..-1].to_sym)
          else
            s(:arg, arg)
          end
        elsif arg.type == :tvar
          on_tvar(arg)
        elsif arg.type == :lasgn
          on_optarg(arg)
        elsif arg.type == :rtype
          on_rtype(arg)
        else
          raise "Unknown arg type #{arg}"
        end
      end

      # Not present in RP output.
      def on_optarg(arg)
        name, default_value = arg.children

        arg.updated(:optional_arg, [
          name, process(default_value)
        ])
      end

      def on_args(node)
        args = node.children.map do |arg|
          on_arg(arg)
        end

        node.updated(nil, args)
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

      def on_ivar(node)
        name, = node.children

        node.updated(nil, [
          s(:symbol, name)
        ])
      end

      def on_iasgn(node)
        name, value = node.children

        node.updated(nil, [
          s(:symbol, name),
          process(value)
        ])
      end

      def on_attrasgn(node)
        process(node.updated(:call))
      end
    end
  end
end
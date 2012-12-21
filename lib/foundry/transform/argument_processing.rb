module Foundry
  class Transform::ArgumentProcessing < AST::Processor
    class Expander
      include AST::SexpBuilder

      def initialize(args_node, is_proc)
        @args_node  = args_node
        @is_proc    = is_proc

        @vars       = {}
        @prologue   = []

        @pre_args, @var_args, @post_args = [], [], []
        @block_arg  = nil
        @has_splat  = false

        @return_type = nil
      end

      def seed_variables
        unless @is_proc
          @vars = {
            :Self  => s(:self_arg),
            :Block => s(:block_arg)
          }
        end
      end

      def expand_typed_arg(node)
        if node.type == :typed_arg
          type, arg = node.children
        else
          arg = node
        end

        [ type, arg ]
      end

      def group_nodes
        state = :pre

        @args_node.children.each do |arg_node|
          _, arg = expand_typed_arg(arg_node)

          case arg.type
          when :arg
            state = :post if state == :var

            if state == :pre
              @pre_args << arg_node
            else
              @post_args << arg_node
            end

          when :optional_arg, :splat_arg
            state = :var
            @var_args << arg_node

            if arg.type == :splat_arg
              @has_splat = true
            end

          when :block_arg
            @block_arg = arg_node

          when :returns
            type, = arg_node.children
            @return_type = type

          else
            raise "Unknown arg type #{arg.inspect}"
          end
        end
      end

      def calculate_arity
        @arity_low = @pre_args.length + @post_args.length

        if @has_splat
          @arity_high = nil
        else
          @arity_high = @arity_low + @var_args.count
        end
      end

      def append_arity_checker
        unless @arity_low == 0 && @arity_high.nil?
          checker =
            s(:check_arity,
              s(:args), @arity_low, @arity_high)

          if @is_proc
            @prologue <<
              s(:if, s(:ivar, s(:self_arg), s(:symbol, :@lambda)),
                checker)
          else
            @prologue << checker
          end
        end
      end

      def arg_unpacker_without_types(arg, index_low, index_high)
        case arg.type
        when :arg, :optional_arg
          name, default_value = arg.children

          value =
            s(:tuple_ref, s(:args),
              index_low)

        when :splat_arg
          name, = arg.children
          default_value = s(:tuple)

          value =
            s(:tuple_slice, s(:args), index_low, index_high)

        else
          raise "Unknown arg type #{arg.inspect}"
        end

        if default_value
          value =
            s(:if,
              s(:tuple_bigger?,
                s(:args), index_low),
              value,
              default_value)
        end

        [ name, value ]
      end

      def arg_unpacker_with_types(node, index_low, index_high)
        type, arg   = expand_typed_arg(node)
        name, value = arg_unpacker_without_types(arg, index_low, index_high)

        if type
          value = s(:coerce, type, value)
        end

        [ name, value ]
      end

      def append_arg_unpacker(arg, index_low, index_high=nil)
        name, value = arg_unpacker_with_types(arg, index_low, index_high)

        if name
          @prologue <<
            s(:lasgn, name, value)
        else
          @prologue <<
            value
        end
      end

      def append_block_arg
        name, = @block_arg.children

        @prologue <<
          s(:lasgn, name, s(:proc_ref))
      end

      def append_arg_unpackers
        @pre_args.each_with_index do |arg, index|
          append_arg_unpacker(arg, index)
        end

        @var_args.each_with_index do |arg, index|
          append_arg_unpacker(arg,
              @pre_args.length + index,
              -(@post_args.length + 1))
        end

        @post_args.each_with_index do |arg, index|
          append_arg_unpacker(arg, -(@post_args.length - index))
        end

        append_block_arg if @block_arg
      end

      def add_epilogue(node)
        if @return_type
          s(:coerce, @return_type, node)
        else
          node
        end
      end

      def process(fun_node, body_nodes)
        seed_variables
        group_nodes

        calculate_arity
        append_arity_checker

        append_arg_unpackers

        fun_body = fun_node.updated(:let, [
          @vars,
          *@prologue,
          *body_nodes
        ])

        add_epilogue(fun_body)
      end
    end

    def expand(node, args_node, body_nodes, is_proc)
      Expander.
        new(args_node, is_proc).
        process(node, body_nodes)
    end

    def on_def(node)
      target, name, args, *body = node.children

      node.updated(nil, [
        process(target), name,
        expand(node, args, process_all(body), false)
      ])
    end

    def on_lambda(node)
      args, *body = node.children

      node.updated(nil, [
        expand(node, args, process_all(body), true)
      ])
    end

  end
end
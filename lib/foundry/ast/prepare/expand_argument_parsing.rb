module Foundry
  module AST::Prepare
    class ExpandArgumentParsing < AST::Processor
      def expand(node, args, body, is_proc)
        if is_proc
          vars = {}
        else
          vars = {
            :Self  => s(:self),
            :Block => s(:proc_ref)
          }
        end

        pre_args, var_args, post_args = [], [], []
        has_splat = false
        block_arg = nil

        state = :pre
        args.children.each do |arg|
          case arg.type
          when :arg
            state = :post if state == :var

            if state == :pre
              pre_args << arg
            else
              post_args << arg
            end

          when :optional_arg, :splat_arg
            state = :var
            var_args << arg

            has_splat = true if arg.type == :splat_arg

          when :block_arg
            block_arg = arg

          else
            raise "Unknown arg type #{arg.inspect}"
          end
        end

        arity_from = pre_args.length + post_args.length

        if has_splat
          arity_to = nil
        else
          arity_to = arity_from
        end

        unpacker = []

        unless arity_from == 0 && arity_to.nil?
          unpacker <<
            s(:check_arity,
              s(:args), arity_from, arity_to)
        end

        pre_args.each_with_index do |arg, index|
          name, = arg.children

          unpacker <<
            s(:lasgn, name,
              s(:array_ref,
                s(:args),
                index))
        end

        var_args.each_with_index do |arg, index|
          this_arg_unpacker = nil

          if arg.type == :optional_arg
            name, default_value = arg.children

            arg_unpacker =
              s(:array_ref, s(:args), pre_args.length + index)

          elsif arg.type == :splat_arg
            name, = arg.children
            default_value = s(:array)

            if name
              arg_unpacker =
                s(:array_slice,
                  s(:args),
                  pre_args.length + index,
                  -(post_args.length + 1))
            end
          end

          if arg_unpacker
            unpacker <<
              s(:lasgn, name,
                s(:if,
                  s(:array_bigger_than,
                    s(:args), arity_from + index),
                  arg_unpacker,
                  default_value))
          end
        end

        post_args.each_with_index do |arg, index|
          name, = arg.children

          unpacker <<
            s(:lasgn, name,
              s(:array_ref,
                s(:args),
                -(post_args.count - index)))
        end

        if block_arg
          name, = block_arg.children

          unpacker <<
            s(:lasgn, name, s(:proc_ref))
        end

        node.updated(:let, [
          vars,
          *unpacker,
          *body
        ])
      end

      def on_def(node)
        target, name, args, *body = node.children

        node.updated(nil, [
          process(target), name,
          expand(node, args, body, false)
        ])
      end

      def on_proc(node)
        args, *body = node.children

        node.updated(nil, [
          expand(node, args, body, true)
        ])
      end
    end
  end
end
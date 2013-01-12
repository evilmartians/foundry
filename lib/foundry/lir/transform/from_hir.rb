module Foundry
  class LIR::Transform::FromHIR < Furnace::AST::Processor
    def initialize(mod)
      @lir_module = mod
    end

    def handler_missing(node)
      raise "cannot lower #{node.type}"
    end

    CONTEXT_VARS = [:Defn, :Cref].freeze

    def run(code, binding, name_prefix='anonymous')
      @name_prefix = name_prefix
      @env         = binding.type.to_static_env

      @builder     = LIR::Builder.new(@name_prefix, [
                        [ nil,                       'self'  ],
                        [ LiteralTupleType.new(nil), 'args'  ],
                        [ Monotype.of(VI::Proc),     'block' ],
                    ], nil)
      @lir_module.add(@builder.function)

      @function = @builder.function
      @self_arg, @args, @block_arg = @function.arguments

      @context = {}

      CONTEXT_VARS.each do |var|
        value = binding.value.apply(var)
        @context[var] = Foundry.constant(value)
      end

      if binding.constant?
        @binding = binding
      else
        @binding = @builder.ivar_load binding.type,
                        [ @self_arg, @builder.symbol(:@binding) ]
      end

      @builder.return process(code)

      @function
    rescue
      $stderr.puts "Failure while processing HIR:\n#{code.inspect}"
      raise
    end

    def on_self_arg(node)
      @self_arg
    end

    def on_args(node)
      @args
    end

    def on_block_arg(node)
      @block_arg
    end

    def on_let(node)
      vars, *body = *node

      old_env, old_binding, old_context = @env, @binding, @context

      @env     = [ (vars.keys - CONTEXT_VARS).to_set ] + @env
      @binding = @builder.binding (vars.keys - CONTEXT_VARS), [ @binding ]

      vars.each do |var, value_node|
        next if value_node.type == :void

        if CONTEXT_VARS.include?(var)
          @context = @context.merge({ var => process(value_node) })
        else
          value = process(value_node)
          @builder.lvar_store 0, var, [ @binding, value ]
        end
      end

      process_all(body).last

    ensure
      @env, @binding, @context = old_env, old_binding, old_context
    end

    def on_var(node)
      name, = *node

      if CONTEXT_VARS.include?(name)
        @context[name]
      else
        @env.each_with_index do |frame, depth|
          if frame.include? name
            return @builder.lvar_load nil, depth, name, [ @binding ]
          end
        end

        raise "cannot find #{name} in environment"
      end
    end

    def on_mut(node)
      name, value_node = *node

      value = process(value_node)

      @env.each_with_index do |frame, depth|
        if frame.include? name
          @builder.lvar_store depth, name, [ @binding, value ]

          return value
        end
      end

      raise "cannot find #{name} in environment"
    end

    def on_eval_mut(node)
      name, value = *node

      @env[0].add name

      on_mut(node)
    end

    def on_ivar(node)
      object, var = *process_all(node)

      @builder.ivar_load nil, [ object, var ]
    end

    def on_imut(node)
      object, var, value = *process_all(node)

      @builder.ivar_store [ object, var, value ]

      value
    end

    def on_nil(node)
      @builder.nil
    end

    def on_false(node)
      @builder.false
    end

    def on_true(node)
      @builder.true
    end

    def on_integer(node)
      value, = *node
      @builder.integer(value)
    end

    def on_symbol(node)
      value, = *node
      @builder.symbol(value)
    end

    def on_string(node)
      value, = *node
      @builder.string(value)
    end

    def on_tuple(node)
      node.children.reduce(@builder.tuple) do |tuple, child|
        if child.type == :splat
          splat_content, = *child
          @builder.tuple_concat [ tuple, process(splat_content) ]
        else
          tuple.operands << process(child)
          tuple
        end
      end
    end

    def on_tuple_ref(node)
      tuple_node, index = *node
      @builder.tuple_ref index, [ process(tuple_node) ]
    end

    def on_tuple_bigger?(node)
      tuple_node, size = *node
      @builder.tuple_bigger size, [ process(tuple_node) ]
    end

    def on_tuple_slice(node)
      tuple_node, from, to = *node
      @builder.tuple_slice from, to, [ process(tuple_node) ]
    end

    def on_const_base(node)
      Foundry.constant(VI::Object)
    end

    def on_const_ref(node)
      cref, constant = *node
      @builder.const_ref constant, [ process(cref) ]
    end

    def on_const_fetch(node)
      scope, constant = *node
      @builder.const_fetch constant, [ process(scope) ]
    end

    def make_closure(code, name_prefix)
      transform = LIR::Transform::FromHIR.new(@lir_module)
      closure   = transform.run(code, @binding, name_prefix)

      @builder.closure [ @binding, closure.to_value ]
    end

    def on_def(node)
      scope_node, name, body_node = *node

      scope = process(scope_node)

      @builder.append LIR::DefineMethod,
                [ scope,
                  LIR::Constant.new(VI::Symbol, name),
                  make_closure(body_node, name) ]

      LIR::Constant.new(VI::NilClass, VI::NIL)
    end

    def on_lambda(node)
      body_node, = *node

      make_closure(body_node, "#{@name_prefix}$l")
    end

    def on_send(node)
      receiver, method, args, block = *process_all(node)

      function = @builder.resolve_method([ receiver, method ])

      @builder.invoke nil,
          [ function, receiver, args, block ]
    end

    def on_apply(node)
      proc, args, block = *process_all(node)

      function = @builder.resolve_closure([ proc ])

      @builder.invoke nil,
          [ function, proc, args, block ]
    end

    def on_block(node)
      process_all(node).last
    end

    #
    # Control flow
    #

    def flip_if(should_flip, branches)
      if should_flip
        branches.reverse
      else
        branches
      end
    end

    def on_if(node)
      cond, if_true, if_false = *node

      @builder.control_flow_op(:branch_if, [ process(cond) ]) do |head, tail|
        [
          @builder.fork(tail) { process(if_true)  },
          @builder.fork(tail) { process(if_false) }
        ]
      end
    end

    def process_logic_shortcut(node, is_or)
      left, right = *node

      left_value = process(left)
      @builder.control_flow_op(:branch_if, [ left_value ]) do |head, tail|
        flip_if(is_or, [
          @builder.fork(tail) { process(right)  },
          [ tail, left_value ]
        ])
      end
    end

    def on_and(node)
      process_logic_shortcut(node, false)
    end

    def on_or(node)
      process_logic_shortcut(node, true)
    end

    def process_loop(node, is_until)
      cond, body = *node

      @builder.add_block do
        cond_value = process(cond)
        @builder.control_flow_op(:branch_if, [ cond_value ]) do |head, tail|
          flip_if(is_until, [
            @builder.fork(head) do
              process(body)
              @builder.nil
            end,

            [ tail, @builder.nil ]
          ])
        end
      end
    end

    def on_while(node)
      process_loop(node, false)
    end

    def on_until(node)
      process_loop(node, true)
    end

    #
    # Primitives
    #

    def on_allocate(node)
      klass, = process_all(node)
      @builder.allocate [ klass ]
    end

    def on_intop(node)
      op_node, left_node, right_node = *node
      op, = *op_node

      @builder.integer_op(op,
          process_all([ left_node, right_node ]))
    end

    def on_trace(node)
      @builder.trace process_all(node)
    end

    #
    # Utilites
    #

    def on_check_arity(node)
      args_node, min, max = *node

      args = process(args_node)

      @builder.check_arity min, max, [ args ]

      args
    end

    def on_check_block(node)
      proc, = *process_all(node)

      @builder.check_block [ proc ]

      proc
    end
  end
end
module Foundry
  class LIR::Transform::FromHIR < Furnace::AST::Processor
    def initialize(mod)
      @lir_module = mod
    end

    def transform(body_node, outer_env, name_prefix='anonymous')
      @name_prefix = name_prefix
      @env         = outer_env

      @builder  = LIR::Builder.new(@name_prefix, [
                      [ VI::Binding,       'outer' ],
                      [ nil,               'self'  ],
                      [ VI::Foundry_Tuple, 'args'  ],
                      [ VI::Proc,          'block' ],
                  ], nil)

      @function = @builder.function
      @binding, @self_arg, @args, @block_arg = @function.arguments

      @lir_module.add(@function)

      @builder.return process(body_node)

      @function
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

      old_env, old_binding = @env, @binding

      @env     = [ vars.keys.to_set ] + @env
      @binding = @builder.binding vars.keys, [ @binding ]

      vars.each do |var, value_node|
        value = process(value_node)

        @builder.lvar_store 0, var, [ @binding, value ]
      end

      process_all(body).last

    ensure
      @env, @binding = old_env, old_binding
    end

    def on_var(node)
      name, = *node

      @env.each_with_index do |frame, depth|
        if frame.include? name
          return @builder.lvar_load nil, depth, name, [ @binding ]
        end
      end

      raise "cannot find #{name} in environment"
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

    def on_true(node)
      @builder.true
    end

    def on_false(node)
      @builder.false
    end

    def on_nil(node)
      @builder.nil
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
      @builder.tuple process_all(node)
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

    def make_lambda(body_node, name_prefix)
      transform = LIR::Transform::FromHIR.new(@lir_module)
      lambda    = transform.transform(body_node, @env, name_prefix)

      @builder.lambda [ @binding, lambda.to_value ]
    end

    def on_def(node)
      scope_node, name, body_node = *node

      scope = process(scope_node)

      @builder.append LIR::DefineMethod,
                [ scope,
                  LIR::Constant.new(VI::Symbol, name),
                  make_lambda(body_node, name) ]

      LIR::Constant.new(VI::NilClass, VI::NIL)
    end

    def on_lambda(node)
      body_node, = *node

      make_lambda(body_node, "#{@name_prefix}$l")
    end

    def on_send(node)
      receiver, method, args, block = *process_all(node)

      method_body = @builder.resolve [ receiver, method ]

      @builder.call nil, [ method_body, receiver, args, block ]
    end

    def on_apply(node)
      proc, args, block = *process_all(node)

      @builder.call nil, [ proc, proc, args, block ]
    end

    def on_block(node)
      process_all(node).last
    end

    #
    # Control flow
    #

    def on_if(node)
      cond, if_true, if_false = *node

      @builder.control_flow_op(:branch_if, [ process(cond) ]) do |post|
        [
          @builder.branch(post) { process(if_true)  },
          @builder.branch(post) { process(if_false) }
        ]
      end
    end

    def on_and(node)
      left, right = *node

      left_value = process(left)
      @builder.control_flow_op(:branch_if, [ left_value ]) do |post|
        [
          @builder.branch(post) { process(right)  },
          [ post, left_value ]
        ]
      end
    end

    def on_or(node)
      left, right = *node

      left_value = process(left)
      @builder.control_flow_op(:branch_if, [ left_value ]) do |post|
        [
          [ post, left_value ],
          @builder.branch(post) { process(right)  }
        ]
      end
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
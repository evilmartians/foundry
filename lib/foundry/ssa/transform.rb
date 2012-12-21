module Foundry
  class SSA::Transform < Furnace::AST::Processor
    def initialize(context)
      @context = context

      @self_arg = SSA::Value.new('self')
      @args     = SSA::Value.new('args')
      @proc_arg = SSA::Value.new('proc')

      @builder = SSA::Builder.new([ @self_arg, @args, @proc_arg ], nil)

      @function_id = @context.make_id
      @context.set(@function_id, @builder.function)
    end

    def transform(ast)
      @builder.return process(ast)

      puts @builder.function.inspect
      #puts @builder.function.to_graphviz

      @function_id
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

    def on_var(node)
      @self_arg # HACK
    end

    def on_true(node)
      SSA::Immediate.new(VI::TrueClass, VI::TRUE)
    end

    def on_false(node)
      SSA::Immediate.new(VI::FalseClass, VI::FALSE)
    end

    def on_nil(node)
      SSA::Immediate.new(VI::NilClass, VI::NIL)
    end

    def on_integer(node)
      value, = *node
      SSA::Immediate.new(VI::Integer, value)
    end

    def on_symbol(node)
      value, = *node
      SSA::Immediate.new(VI::Symbol, value)
    end

    def on_tuple(node)
      result, = @builder.insn SSA::Tuple, *process_all(node)
      result
    end

    def on_send(node)
      result, = @builder.insn SSA::Send, *process_all(node)
      result
    end

    def on_block(node)
      process_all(node).last
    end

    def on_if(node)
      cond, if_true, if_false = *node

      true_block, false_block = @builder.cond SSA::If, process(cond)
      post_block = @builder.block

      @builder.goto true_block
      true_value = @builder.assign process(if_true)
      @builder.jump post_block

      @builder.goto false_block
      false_value = @builder.assign process(if_false)
      @builder.jump post_block

      @builder.goto post_block
      @builder.phi true_value, false_value
    end
  end
end
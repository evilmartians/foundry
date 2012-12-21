module Foundry
  class AST::Processor < Furnace::AST::Processor
    include AST::SexpBuilder

    alias transform process

    def handler_missing(node)
      unless node.children.all? { |c| c.is_a? AST::Node }
        $stderr.puts "Failsafe: #{node}"
        return
      end

      node.updated(nil, process_all(node.children))
    end

    #
    # Literals
    #

    def on_literal(node)
      # Leave it alone
    end
    alias on_symbol  on_literal
    alias on_integer on_literal
    alias on_float   on_literal
    alias on_string  on_literal

    #
    # Local variables
    #

    def on_let(node)
      vars, *body = node.children
      node.updated(nil, [ vars, *process_all(body) ])
    end

    def on_var(node)
      # Leave it alone
    end
    alias on_lvar on_var

    def on_mut(node)
      name, value = node.children
      node.updated(nil, [ name, process(value) ])
    end
    alias on_lasgn on_mut

    #
    # Module definitions
    #

    def on_define_class(node)
      scope, name, superclass = node.children
      node.updated(nil, [ process(scope), name, process(superclass) ])
    end

    def on_define_module(node)
      scope, name = node.children
      node.updated(nil, [ process(scope), name ])
    end

    #
    # Function definitions
    #

    def on_def(node)
      scope, name, *body = node.children
      node.updated(nil, [ process(scope), name, *process_all(body) ])
    end

    def on_args(node)
      # Leave it alone
    end

    #
    # Tuples
    #

    def on_tuple(node)
      node.updated(nil, process_all(node.children))
    end

    def on_tuple_ref(node)
      tuple, index = node.children
      node.updated(nil, [
        process(tuple), index
      ])
    end

    def on_tuple_slice(node)
      tuple, index_low, index_high = node.children
      node.updated(nil, [
        process(tuple), index_low, index_high
      ])
    end

    def on_tuple_concat(node)
      left, right = node.children
      node.updated(nil, [
        process(left), process(right)
      ])
    end

    def on_tuple_bigger?(node)
      tuple, length = node.children
      node.updated(nil, [
        process(tuple), length
      ])
    end

    #
    # Constants
    #

    def on_const_ref(node)
      scope, name = node.children
      node.updated(nil, [ process(scope), name ])
    end

    def on_const_fetch(node)
      scope, name = node.children
      node.updated(nil, [ process(scope), name ])
    end

    def on_const_declare(node)
      scope, name, value = node.children
      node.updated(nil, [ process(scope), name, process(value) ])
    end

    #
    # Control flow
    #

    def on_if(node)
      cond, true_branch, false_branch = node.children

      true_branch  = s(:nil) if true_branch.nil?
      false_branch = s(:nil) if false_branch.nil?

      node.updated(nil, [
        process(cond),
        process(true_branch), process(false_branch)
      ])
    end

    def on_loop(node)
      cond, body = node.children
      node.updated(nil, [
        process(cond), process(body)
      ])
    end

    alias on_while on_loop
    alias on_until on_loop

    #
    # Utilites
    #

    def on_check_arity(node)
      # Leave it alone
    end

    def on_check_block(node)
      # Leave it alone
    end
  end
end
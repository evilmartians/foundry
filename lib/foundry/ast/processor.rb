module Foundry
  class AST::Processor < Furnace::AST::Processor
    alias transform process

    def Node(type, children=[], metadata={})
      AST::Node.new(type, children, metadata)
    end

    def on_class(node)
      scope, name, superclass, *code = node.children
      node.updated(nil, [
        process(scope), name, process(superclass),
        *process_all(code)
      ])
    end

    def on_module(node)
      scope, name, *code = node.children
      node.updated(nil, [
        process(scope), name,
        *process_all(code)
      ])
    end

    def on_defn(node)
      name, args, *code = node.children
      node.updated(nil, [ name, process(args), *process_all(code) ])
    end

    def on_block(node)
      node.updated(nil, process_all(node.children))
    end
    alias on_toplevel_block on_block
    alias on_eval_block     on_block

    def on_call(node)
      receiver, name, args = node.children
      node.updated(nil, [ process(receiver), name, process(args) ])
    end

    def on_array(node)
      node.updated(nil, process_all(node.children))
    end

    def on_alias(node)
      node.updated(nil, process_all(node.children))
    end

    def on_const_fetch(node)
      scope, name = node.children
      node.updated(nil, [ process(scope), name ])
    end

    def on_const_declare(node)
      scope, name, value = node.children
      node.updated(nil, [ process(scope), name, process(value) ])
    end

    def process_let(node, flush_env)
      vars, *body = node.children
      node.updated(nil, [ vars, process_all(body) ])
    end

    def on_let_new(node)
      process_let(node, true)
    end

    def on_let(node)
      process_let(node, true)
    end

    def on_mut!(node)
      name, value = node.children
      node.updated(nil, [ name, process(value) ])
    end
    alias on_lasgn on_mut!
  end
end
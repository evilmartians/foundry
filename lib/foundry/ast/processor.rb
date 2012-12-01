module Foundry
  class AST::Processor < Furnace::AST::Processor
    alias transform process

    def s(type, *children)
      AST::Node.new(type, children)
    end

    def on_block(node)
      node.updated(nil, process_all(node.children))
    end
    alias on_toplevel_block on_block
    alias on_eval_block     on_block

    def on_def(node)
      scope, name, *body = node.children
      node.updated(nil, [ process(scope), name, *process_all(body) ])
    end

    def on_proc(node)
      *body = node.children
      node.updated(nil, process_all(body))
    end

    def on_call(node)
      receiver, name, args, block = node.children
      node.updated(nil, [ process(receiver), name, process(args), process(block) ])
    end

    def on_let(node)
      vars, *body = node.children
      node.updated(nil, [ vars, *process_all(body) ])
    end

    def on_mut!(node)
      name, value = node.children
      node.updated(nil, [ name, process(value) ])
    end
    alias on_lasgn on_mut!

    def on_iasgn(node)
      scope, name, value = node.children
      node.updated(nil, [ process(scope), process(name), process(value) ])
    end

    def on_array(node)
      node.updated(nil, process_all(node.children))
    end

    def on_splat(node)
      value, = node.children
      node.updated(nil, [ process(value) ])
    end

    def on_alias(node)
      from, to = node.children
      node.updated(nil, [ process(from), process(to) ])
    end

    def on_const_fetch(node)
      scope, name = node.children
      node.updated(nil, [ process(scope), name ])
    end

    def on_const_declare(node)
      scope, name, value = node.children
      node.updated(nil, [ process(scope), name, process(value) ])
    end

    def on_if(node)
      cond, true_branch, false_branch = node.children
      node.updated(nil, [
        process(cond),
        process(true_branch), process(false_branch)
      ])
    end

    def on_while(node)
      cond, body = node.children
      node.updated(nil, [
        process(cond), process(body)
      ])
    end

    alias on_until on_while

    # Temporary

    def on_singleton_class_of(node)
      obj, = node.children
      node.updated(nil, [ process(obj) ])
    end
  end
end
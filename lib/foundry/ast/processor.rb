module Foundry
  class AST::Processor < Furnace::AST::Processor
    alias transform process

    def on_class(node)
      name, superclass, code = node.children
      node.updated(nil, [ name, process(superclass), process(code) ])
    end

    def on_module(node)
      name, code = node.children
      node.updated(nil, [ name, process(code) ])
    end

    def on_defn(node)
      name, args, code = node.children
      node.updated(nil, [ name, process(args), process(code) ])
    end

    def on_block(node)
      node.updated(nil, process_all(node.children))
    end

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

    def on_const_declare(node)
      name, value = node.children
      node.updated(nil, [ name, process(value) ])
    end

    def on_lasgn(node)
      name, value = node.children
      node.updated(nil, [ name, process(value) ])
    end
  end
end
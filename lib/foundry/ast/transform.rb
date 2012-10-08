module Foundry::AST
  class Transform
    include Furnace::AST::StrictVisitor

    alias transform visit

    def on_class(node)
      name, superclass, code = node.children
      node.update(:class, [ name, visit(superclass), visit(code) ])
    end

    def on_module(node)
      name, code = node.children
      node.update(:module, [ name, visit(code) ])
    end

    def on_defn(node)
      name, args, code = node.children
      node.update(:defn, [ name, visit(args), visit(code) ])
    end

    def on_block(node)
      node.update(:block, visit_all(node.children))
    end
  end
end
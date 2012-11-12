module Foundry
  module AST::Prepare
    class ExpandPrimitives < AST::Transform
      def on_call(node)
        receiver, name, arguments = node.children
        if name == :primitive &&
            receiver.type == :const_lookup &&
            receiver.children.first == :Foundry

          primitive, *primitive_args = arguments.children
          node.updated(primitive.children.first,
            primitive_args)
        end
      end
    end
  end
end
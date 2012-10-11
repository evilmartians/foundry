module Foundry
  module AST::Prepare
    class ExpandPrimitives < AST::Transform
      def on_call(node)
        receiver, name, arguments = node.children
        if name == :primitive &&
            receiver.type == :const &&
            receiver.children.first == :Foundry

          primitive, *primitive_args = arguments.children
          node.update(primitive.children.first,
            primitive_args)
        end
      end
    end
  end
end
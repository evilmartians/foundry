module Foundry::AST
  module Prepare
    class ExpandPrimitives < Transform
      def on_call(node)
        receiver, name, arguments = node.children
        if name == :primitive &&
            receiver.type == :const &&
            receiver.children.first == :Foundry

          primitive, primitive_args = arguments.children
          node.update(:primitive, [
            primitive.children.first,
            arguments.update(nil,
              arguments.children.drop(1))
          ])
        end
      end
    end
  end
end
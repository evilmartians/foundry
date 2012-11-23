module Foundry
  module AST::Prepare
    class ExpandPrimitives < AST::Processor
      def on_call(node)
        receiver, name, arguments = node.children

        if name == :primitive &&
            receiver.type == :const_ref_in &&
            receiver.children.last == :Foundry

          primitive, *primitive_args = arguments.children
          primitive_name, = primitive.children

          node.updated(primitive_name, primitive_args)
        end
      end
    end
  end
end
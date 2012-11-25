module Foundry
  module AST::Prepare
    class ExpandPrimitives < AST::Processor
      def on_call(node)
        receiver, name, arguments = node.children

        if receiver.type == :const_ref_in &&
             receiver.children.last == :FoundryRt

          node.updated(name, arguments.children)
        end
      end
    end
  end
end
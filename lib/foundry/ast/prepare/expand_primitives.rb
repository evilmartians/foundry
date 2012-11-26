module Foundry
  module AST::Prepare
    class ExpandPrimitives < AST::Processor
      def on_call(node)
        receiver, name, arguments, block = node.children

        if receiver.type == :const_ref_in &&
             receiver.children.last == :FoundryRt

          if block.type == :nil
            node.updated(name, arguments.children)
          else
            node.updated(name, arguments.children + [ process(block) ])
          end
        end
      end
    end
  end
end
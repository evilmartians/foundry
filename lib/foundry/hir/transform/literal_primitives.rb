module Foundry
  class HIR::Transform::LiteralPrimitives < HIR::Processor
    def on_send(node)
      receiver_node, name_node, arguments_node, block_node = node.children

      if receiver_node.type == :const_ref &&
           receiver_node.children.last == :FoundryRt

        name, = name_node.children

        if block_node.type == :nil
          node.updated(name, arguments_node.children)
        else
          node.updated(name, arguments_node.children +
              [ process(block_node) ])
        end
      else
        node.updated(nil, process_all(node))
      end
    end

  end
end

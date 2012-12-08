module Foundry
  module Transform::RubyParser::Interpolation
    def process_tuple(nodes)
      result_nodes = []

      nodes.each do |node|
        if node.type == :splat
        else
          result_nodes << node
        end
      end

      s(:tuple, result_nodes)
    end

    def on_tuple(node)
      process_tuple(node.children)
    end
  end
end
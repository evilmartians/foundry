module Foundry
  module Transform::RubyParser::Interpolation
    def process_tuple(nodes)
      result_nodes = []

      nodes.each do |node|
        if node.type == :splat
          result_nodes += node.children
        else
          result_nodes << node
        end
      end

      s(:tuple, result_nodes)
    end

    #def on_tuple(node)
    #  process_tuple(node.children)
    #end

    def process_dstr(seed, nodes)
      nodes.reduce(s(:string, seed)) do |ast, node|
        if node.type == :str
          s_send(ast, :+, node.updated(:string))
        elsif node.type == :evstr
          expr, = *node
          s_send(ast, :+, s_send(process(expr), :to_s))
        end
      end
    end

    def on_dstr(node)
      seed, *append = *node
      process_dstr(seed, append)
    end
  end
end
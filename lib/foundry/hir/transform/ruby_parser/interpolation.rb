module Foundry
  module HIR::Transform::FromRubyParser::Interpolation
    def process_tuple(nodes)
      last_tuple  = []
      concat_node = nil

      nodes.each do |node|
        if node.type == :splat
          unless concat_node
            concat_node = []
          end

          splat_value, = *node

          concat_node << s(:tuple, *last_tuple)
          concat_node << process(splat_value)
          last_tuple  =  []
        else
          last_tuple  << process(node)
        end
      end

      if concat_node.nil?
        s(:tuple, *last_tuple)
      else
        concat_node << s(:tuple, *last_tuple)
        s(:tuple_concat, *concat_node)
      end
    end

    def on_tuple(node)
      process_tuple(node.children)
    end

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
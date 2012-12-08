module Foundry
  module Transform::RubyParser::Literals
    def on_lit(node)
      value, = node.children

      case value
      when Integer
        node.updated(:integer)
      when Symbol
        node.updated(:symbol)
      when Float
        node.updated(:float)
      else
        node
      end
    end

    def on_str(node)
      node.updated(:string)
    end

    def on_dstr(node)
      node.updated(:string, [ "TODO" ])
    end

    def on_array(node)
      process(node.updated(:tuple))
    end
  end
end
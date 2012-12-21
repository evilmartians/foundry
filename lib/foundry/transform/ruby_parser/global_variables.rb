module Foundry
  module Transform::RubyParser::GlobalVariables
    def on_gvar(node)
      name, = *node

      node.updated(:gvar, [
        s(:symbol, name)
      ])
    end

    def on_gasgn(node)
      name, value = *node

      node.updated(:gasgn, [
        s(:symbol, name), process(value)
      ])
    end
  end
end
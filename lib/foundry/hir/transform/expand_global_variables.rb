module Foundry
  class HIR::Transform::ExpandGlobalVariables < HIR::Processor
    def on_gvar(node)
      var_name, = *node

      s_send(
        s_const('Foundry::Globals'),
        :get, var_name)
    end

    def on_gasgn(node)
      var_name, value = *node

      s_send(
        s_const('Foundry::Globals'),
        :set, var_name, value)
    end
  end
end
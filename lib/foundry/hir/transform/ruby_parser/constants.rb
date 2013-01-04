module Foundry
  module HIR::Transform::FromRubyParser::Constants
    def process_constant_declaration(what)
      if what.is_a? Symbol
        scope = s(:const_declare_scope)
        name  = what
      elsif what.type == :colon2
        scope, name = what.children
        scope = process(scope)
      elsif what.type == :colon3
        name, = what.children
        scope = s(:const_base)
      end

      [ process(scope), name ]
    end

    def on_const(node)
      process(node.updated(:const_search))
    end

    def on_colon2(node)
      scope, name = node.children

      process(node.updated(:const_fetch, [
        scope, name
      ]))
    end

    def on_colon3(node)
      name, = node.children

      process(node.updated(:const_fetch, [
        s(:const_base), name
      ]))
    end

    def on_cdecl(node)
      name, value = node.children

      process(node.updated(:const_declare, [
        *process_constant_declaration(name),
         value
      ]))
    end
  end
end
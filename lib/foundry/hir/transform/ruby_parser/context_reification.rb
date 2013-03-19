module Foundry
  module HIR::Transform::FromRubyParser::ContextReification
    def process_toplevel_block(node)
      if @is_eval
        node.updated(:block,
          process_all(node.children),
          function: '(eval)')
      else
        node.updated(:let, [
          {
            :self     => s(:self_arg),
            :"&block" => s(:nil),
            :Defn     => s(:const_base),
            :Cref     => s(:tuple,
                           s(:const_base)),
          },
          *process_all(node.children),
        ], function: '(toplevel)')
      end
    end

    def process_module_scope(node, new_scope, body, function)
      node.updated(:let, [
        {
          :Scope  => new_scope,
        },
        s(:let, {
            :self     => s(:var, :Scope),
            :"&block" => s(:nil),
            :Defn     => s(:var, :Scope),
            :Cref     => s(:tuple_concat,
                           s(:tuple, s(:var, :Scope)),
                           s(:var, :Cref)),

          },
          *process_all(body))
      ], function: function)
    end

    def on_class(node)
      name, superclass, *body = *node

      superclass = s(:nil) if superclass.nil?

      process_module_scope(node,
        s(:define_class,
          *process_constant_declaration(name),
           process(superclass)),
        body,
        '<class body>')
    end

    def on_module(node)
      name, *body = *node

      process_module_scope(node,
        s(:define_module,
          *process_constant_declaration(name)),
        body,
        '<module body>')
    end

    def on_sclass(node)
      scope, *body = *node

      process_module_scope(node,
        s(:singleton_class_of,
          process(scope)),
        body,
        '<singleton class body>')
    end

    def on_const_search(node)
      name, = *node

      node.updated(:const_ref, [
        s(:var, :Cref),
        name
      ])
    end

    def on_const_declare_scope(node)
      s(:tuple_ref,
        s(:var, :Cref),
        0)
    end

    def on_defn(node)
      name, args, *code = *node

      node.updated(:def, [
        s(:var, :Defn),
         name,
         process(args),
        *process_all(code)
      ])
    end

    def on_defs(node)
      target, name, args, *code = *node

      node.updated(:def, [
        s(:singleton_class_of, process(target)),
         name,
         process(args),
        *process_all(code)
      ])
    end

    def on_self(node)
      node.updated(:var, [ :self ])
    end

    def on_yield(node)
      node.updated(:apply, [
        s(:check_block, s(:var, :"&block")),
        process(node.updated(:array)),
        s(:nil)
      ])
    end

    def on_zsuper(node)
      node.updated(:super, [
        s(:args)
      ])
    end

    def on_ivar(node)
      name, = *node

      node.updated(:ivar, [
        s(:var, :self),
        s(:symbol, name)
      ])
    end

    def on_iasgn(node)
      name, value = *node

      node.updated(:imut, [
        s(:var, :self),
        s(:symbol, name),
        process(value)
      ])
    end
  end
end

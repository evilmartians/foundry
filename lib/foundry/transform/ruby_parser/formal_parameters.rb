module Foundry
  module Transform::RubyParser::FormalParameters
    def process_typed_arg(node)
      klass, declaration = node.children

      node.updated(:typed_arg, [
        process(klass), process_arg(declaration)
      ])
    end

    def process_return_type(node)
      type, = node.children

      node.updated(:returns, [
        process(type)
      ])
    end

    def process_optional_arg(arg)
      name, default_value = arg.children

      arg.updated(:optional_arg, [
        name, process(default_value)
      ])
    end

    def process_arg(arg)
      if arg.is_a? Symbol
        if arg == :*
          s(:splat_arg)
        elsif arg[0] == '*'
          s(:splat_arg, arg[1..-1].to_sym)
        elsif arg[0] == '&'
          s(:block_arg, arg[1..-1].to_sym)
        else
          s(:arg, arg)
        end
      elsif arg.type == :tvar
        process_typed_arg(arg)
      elsif arg.type == :lasgn
        process_optional_arg(arg)
      elsif arg.type == :rtype
        process_return_type(arg)
      else
        raise "Unknown arg type #{arg}"
      end
    end

    def on_args(node)
      node.updated(nil,
        node.children.map { |arg| process_arg(arg) })
    end
  end
end
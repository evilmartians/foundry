module Foundry
  module HIR::SexpBuilder
    def s(type, *children)
      HIR::Node.new(type, children)
    end

    def s_const(name)
      name.split('::').reduce(nil) do |ast, name_part|
        if name_part.empty?
          s(:const_base)
        elsif ast.nil?
          s(:const_ref, s(:var, :Cref), name_part.to_sym)
        else
          s(:const_fetch, ast, name_part.to_sym)
        end
      end
    end

    def s_send(receiver, method, *args)
      s(:send,
        receiver,
        s(:symbol, method),
        s(:tuple, *args),
        s(:nil))
    end
  end
end
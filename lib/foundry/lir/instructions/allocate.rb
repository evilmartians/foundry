module Foundry
  class LIR::AllocateInsn < Furnace::SSA::Instruction
    syntax do |s|
      s.operand :klass#, Type.klass(VI::Class)
    end

    def type
      # TODO make this handle classes without associated singletons.
      if !klass.type.variable? &&
            klass.type.klass.is_a?(VI::SingletonClass)
        Type.klass(klass.type.klass.object)
      else
        Type.top
      end
    end
  end
end

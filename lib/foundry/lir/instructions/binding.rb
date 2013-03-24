module Foundry
  class LIR::BindingInsn < Furnace::SSA::GenericInstruction
    syntax do |s|
      s.operand :next
    end

    def initialize(variables=[], operands=[], name=nil)
      @variables = []

      super(Type.bottom, operands, name)

      @variables = variables
      self.type  = Type::Binding.new(
                      @variables.map { |k| [k, Type.variable] },
                      self.next.type)
    end

    def remove_variable(var)
      @variables.delete var

      self.type  = Type::Binding.new(
                      self.type.variables.reject do |name, |
                        name == var
                      end,
                      self.type.next)
    end

    def remove_next
      self.next  = Foundry.constant VI::NIL
      self.type  = Type::Binding.new(
                      self.type.variables,
                      nil)
    end

    def awesome_print_parameters(p=Furnace::AwesomePrinter.new)
      p.text    @variables.map(&:inspect).join(", ")
      p.keyword 'chain'
    end
  end
end

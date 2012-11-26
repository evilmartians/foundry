module Foundry::Interpreter
  class Ruby < Base
    #
    # Objects
    #

    def on_trace(node)
      $stderr.puts process_all(node.children).map(&:inspect).join(", ")

      Foundry::VI::NIL
    end

    def on_equal?(node)
      self_, other = process_all(node.children)

      if self_.class == other.class &&
            (self_.class == VI::Integer ||
             other.class == VI::Symbol)
        self_.value == other.value ? VI::TRUE : VI::FALSE
      else
        self_.equal?(other) ? VI::TRUE : VI::FALSE
      end
    end

    #
    # Classes and modules
    #

    def on_allocate(node)
      self_, = process_all(node.children)
      self_.vm_allocate
    end

    #
    # Integers
    #

    {
      :+  => :add,
      :-  => :sub,
      :*  => :mul,
      :/  => :div,
    }.each do |op, node|
      define_method(:"on_int_#{node}") do |node|
        self_, other = process_all(node.children)
        VI.new_integer(self_.value.send(op, other.value))
      end
    end

    {
      :<  => :lt,
      :<= => :lte,
      :>  => :gt,
      :>= => :gte,
    }.each do |op, node|
      define_method(:"on_int_#{node}") do |node|
        self_, other = process_all(node.children)
        self_.value.send(op, other.value) ? VI::TRUE : VI::FALSE
      end
    end
  end
end
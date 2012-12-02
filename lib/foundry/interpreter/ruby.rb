module Foundry::Interpreter
  class Ruby < Base
    #
    # Classes
    #

    def on_class_of(node)
      self_, = node.children
      process(self_).class
    end

    def on_is_a?(node)
      self_, klass = node.children
      process(self_).is_a?(process(klass)) ? VI::TRUE : VI::FALSE
    end

    def on_singleton_class_of(node)
      self_, = node.children
      process(self_).singleton_class
    end

    #
    # Objects
    #

    def on_trace(node)
      results = process_all(node.children)
      $stderr.puts results.map(&:inspect).join(", ")

      if results.one?
        results.first
      else
        VI.new_tuple(results)
      end
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
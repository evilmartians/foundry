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

    # TODO
    include Foundry::AST::SexpBuilder

    def on_coerce(node)
      type, value = node.children
      on_call(s(:send, type, :coerce, s(:array, value), s(:nil)))
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
    # LUTs
    #

    def on_lut_lookup(node)
      self_, key = process_all(node.children)
      self_[key.value]
    end

    def on_lut_store(node)
      self_, key, value = process_all(node.children)
      self_[key.value] = value
    end

    def on_lut_check(node)
      self_, key = process_all(node.children)
      self_.key?(key.value) ? VI::TRUE : VI::FALSE
    end

    def on_lut_traverse(node)
      self_, proc = process_all(node.children)

      self_.each do |key, value|
        proc.call(VI::NIL, VI.new_tuple([ VI.new_symbol(key), value ]), VI::NIL, self)
      end

      self_
    end

    #
    # Tuples
    #

    def on_tuple_size(node)
      self_, = process_all(node.children)
      VI.new_integer self_.size
    end

    def on_tuple_lookup(node)
      self_, index = process_all(node.children)
      self_[index.value]
    end

    #
    # Integers
    #

    {
      :+  => :add,
      :-  => :sub,
      :*  => :mul,
      :/  => :div,
      :%  => :mod,
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
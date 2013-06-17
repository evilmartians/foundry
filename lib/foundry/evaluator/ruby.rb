module Foundry::Evaluator
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

    def on_new_class(node)
      superclass, = *process_all(node)
      VI.new_class(superclass)
    end

    def on_define_method(node)
      klass, name, block = *process_all(node)

      method = VI.new_proc(
          Foundry::HIR::Node.new(
            :let,
            [
              {
                :self     => Foundry::HIR::Node.new(:self_arg),
                :"&block" => Foundry::HIR::Node.new(:block_arg)
              },
              block.code.
                updated(nil, nil, function: "(define_method:#{name.to_sym})")
            ]),
          block.binding)

      klass.define_method(name.to_sym, method)

      VI::NIL
    end

    #
    # Objects
    #

    def on_trace(node)
      result = process(node.children.first)
      $stderr.puts result.inspect

      VI::NIL
    end

    def on_equal?(node)
      self_, other = process_all(node.children)

      if self_.class == other.class &&
            other.class == VI::Symbol
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
    # Primitives
    #

    def on_intop(node)
      op, left, right = *process_all(node)
      case op.value
      when :<, :<=, :>, :>=, :==, :!=
        left.value.send(op.value, right.value) ?
            VI::TRUE : VI::FALSE

      when :+, :-, :*, :/, :%, :**
        if left.width != right.width ||
              left.signed? != right.signed?
          raise Error.new(self, "cannot coerce #{left.inspect} and #{right.inspect}")
        end

        VI.new_machine_integer(
            left.value.send(op.value, right.value),
            left.signed?,
            left.width)

      when :<<
        VI.new_machine_integer(
            left.value.send(op.value, right.value),
            left.signed?,
            left.width + right.value)

      when :>>
        if left.width < right.value
          raise Error.new(self, "cannot shift #{left.inspect} by #{right} to the right")
        end

        VI.new_machine_integer(
            left.value.send(op.value, right.value),
            left.signed?,
            left.width - right.value)

      when :to_s
        VI.new_string left.value.to_s
      end
    end

    def on_int_convert(node)
      source, signed, width = *process_all(node)

      VI.new_machine_integer(
          source.to_int,
          signed == VI::TRUE,
          width.to_int)
    end

    def on_symop(node)
      op, left, right = *process_all(node)
      case op.value
      when :to_s
        VI.new_string left.value.to_s
      end
    end

    def on_strop(node)
      op, left, right = *process_all(node)
      case op.value
      when :+
        VI.new_string left.value + right.value
      when :to_sym
        VI.new_symbol left.value
      end
    end
  end
end

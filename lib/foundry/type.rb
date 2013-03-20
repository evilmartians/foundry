module Foundry
  module Type
    include Furnace::Type

    def self.top
      Type::Top.new
    end

    def self.bottom
      Type::Bottom.new
    end

    def self.variable
      Type::Variable.new
    end

    def self.value(object)
      Type::Value.new(object)
    end

    def self.boolean
      Type::Ruby.new(VI::Object, {})
    end

    def self.klass(klass)
      if klass.unreified == VI::Machine_Integer
        Type::MachineInteger.new(
            Type.value(klass.specializations[VMSymbol.new(:signed)]),
            Type.value(klass.specializations[VMSymbol.new(:width)]))
      else
        Type::Ruby.new(klass, klass.specializations)
      end
    end

    def self.of(object)
      if object.__vm_object?
        if object.singleton_class_defined?
          klass = object.singleton_class
        else
          klass = object.class
        end

        if klass == VI::Tuple
          Type::Tuple.new(object.to_a.map { |obj| Type.of(obj) })
        elsif klass == VI::Binding
          object.__type__ # TODO refactor this somehow
        elsif klass == VI::Proc
          Type::Closure.new(klass)
        else
          Type.klass(klass)
        end
      else
        case object
        when LIR::Value
          object.type
        else
          raise ArgumentError, "Type.of(#{object.class}) is not defined"
        end
      end
    end
  end

  require_relative 'type/tuple'
  require_relative 'type/binding'
  require_relative 'type/closure'
  require_relative 'type/ruby'
  require_relative 'type/lookup_table'
  require_relative 'type/machine_integer'
end

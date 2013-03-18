module Foundry
  class Type::Ruby < Type::Top
    attr_reader :klass, :specializations

    def initialize(klass, specializations)
      @klass           = klass
      @specializations = Hash[
        specializations.to_hash.map do |key, value|
          [ key.to_sym, value_to_type(value) ]
        end].freeze
    end

    def subtype_of?(type)
      super ||
          type.instance_of?(Type::Ruby) &&
            klass.ancestors.include?(type.klass) &&
            specialization_compatible_subtype?(type.specializations)
    end

    def to_klass
      @klass
    end

    def to_s
      LIR::PrettyPrinter.new(false) do |p|
        pretty_print(p)
      end
    end

    def pretty_print(p=LIR::PrettyPrinter.new)
      if @klass.is_a? VI::SingletonClass
        p.type "singleton"
        p <<   '<'
        p.text @klass.object.inspect
        p <<   '>'
      else
        if @specializations[:by_value] == Type.value(VI::TRUE)
          p.type "#{@klass.name}&"
        else
          p.type @klass.name
        end

        if (@specializations.keys - [:by_value]).any?
          p << '<'
          @specializations.each do |key, value|
            p.text key
            p <<   ':'
            value.pretty_print p
          end
          p.text '>'
        end
      end

      p
    end

    protected

    def value_to_type(value)
      if value.is_a? VI::Class
        Type.klass(value)
      else
        Type.value(value)
      end
    end

    def specialization_compatible_subtype?(specializations)
      specializations.keys.map do |param|
        specializations[param] == @parameters[param]
      end.reduce(:&)
    end
  end
end

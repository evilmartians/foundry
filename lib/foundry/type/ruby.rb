module Foundry
  class Type::Ruby < Type::Top
    attr_reader :klass, :parameters

    def initialize(klass, parameters)
      @klass      = klass
      @parameters = parameters.freeze
    end

    def subtype_of?(type)
      super ||
          type.instance_of?(Type::Ruby) &&
            klass.ancestors.include?(type.klass) &&
            parameter_compatible_subtype?(type.parameters)
    end

    def to_s
      if @klass.is_a? VI::SingletonClass
        "singleton<#{@klass.object.inspect}>"
      else
        "#{@klass.name.to_s}*"
      end
    end

    protected

    def parameter_compatible_subtype?(parameters)
      parameters.keys.map do |param|
        parameters[param] == @parameters[param]
      end.reduce(:&)
    end
  end
end

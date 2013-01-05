module Foundry
  class ::BasicObject
    def __vm_object?
      false
    end
  end

  class VMImmediate < ::BasicObject
    def __vm_object?
      true
    end

    def nil?
      equal? VI::NIL
    end

    def to_s
      inspect
    end

    def type
      self.class
    end

    def singleton_class
      self.class
    end

    def singleton_class_defined?
      false
    end

    def is_a?(klass)
      if singleton_class_defined?
        self.singleton_class.ancestors.include? klass
      else
        self.class.ancestors.include? klass
      end
    end

    def respond_to?(method)
      if singleton_class_defined?
        self.singleton_class.method_defined? method
      else
        self.class.method_defined? method
      end
    end

    def method(method)
      if singleton_class_defined?
        self.singleton_class.instance_method method
      else
        self.class.instance_method method
      end
    end

    def instance_variables
      []
    end

    def instance_variable_get(ivar)
      VI::NIL
    end

    def instance_variable_set(ivar, value)
      VI::NIL
    end

    def self.define_mapped_ivars(*set)
      set = set.map { |var| :"@#{var}" }

      # This basically adjusts cref to include receiver class.
      # Also, note that VMImmediate descends from BasicObject
      # which means that there is no host instance_variable_set.
      class_eval <<-EVAL, __FILE__ + '/define_mapped_ivars'
      def instance_variables
        VI.new_tuple(#{set.inspect} + super.to_a)
      end

      def instance_variable_get(ivar)
        #{set.each_with_index.map do |ivar, index| <<-IVAR
        #{'els' if index > 0}if ivar == #{ivar.inspect}
          #{ivar} || VI::NIL
          IVAR
        end.join}
        else
          super
        end
      end

      def instance_variable_set(ivar, value)
        #{set.each_with_index.map do |ivar, index| <<-IVAR
        #{'els' if index > 0}if ivar == #{ivar.inspect}
          #{ivar} = value
          IVAR
        end.join}
        else
          super
        end
      end
      EVAL
    end

    def inspect
      if @ivar_table.size > 0
        ivs = " "
        @ivar_table.each do |key, value|
          ivs << "#{key}=#{value.inspect}"
        end
      end

      "{#{@class.name}:#{__id__}#{ivs}}"
    end
  end
end
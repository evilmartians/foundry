module Foundry
  class VMModule < VMObject
    attr_reader :name
    attr_reader :upperclass

    attr_reader :const_table, :method_table
    protected   :const_table, :method_table

    def initialize(klass, name=nil)
      super(klass)

      @name         = name
      @upperclass   = nil
      @const_table  = {}
      @method_table = {}
    end

    def include(modulus)
      @upperclass = VI::Foundry_IncludedModule.allocate(modulus, @upperclass)
    end

    def _include(interp, scope)
      scope.self.include(*scope.arguments)
    end

    def ancestors
      if @upperclass
        [ self ] + @upperclass.ancestors
      else
        [ self ]
      end
    end

    def constants(search_parent=true)
      if @upperclass
        (@const_table.keys + @upperclass.constants).uniq
      else
        @const_table.keys
      end
    end

    def const_defined?(const, search_parent=true)
      if exists = @const_table.key?(const)
        exists
      elsif search_parent && @upperclass
        @upperclass.const_defined? const
      else
        false
      end
    end

    def const_set(const, value)
      @const_table[const] = value
    end

    def const_get(const, search_parent=true)
      if value = @const_table[const]
        value
      elsif search_parent && @upperclass
        @upperclass.const_get const
      else
        VI::UNDEF
      end
    end

    def instance_methods(search_parent=true)
      if search_parent && @upperclass
        (@method_table.keys + @upperclass.instance_methods).uniq
      else
        @method_table.keys
      end
    end

    def method_defined?(method, search_parent=true)
      if @method_table.key?(method) &&
            !((undefined = @method_table[method]) == VI::UNDEF)
        true
      elsif !undefined && search_parent && @upperclass
        @upperclass.method_defined?(method)
      else
        false
      end
    end

    def define_method(method, value)
      @method_table[method] = value
    end

    def instance_method(method, search_parent=true)
      if value = @method_table[method]
        value
      elsif search_parent && @upperclass
        @upperclass.instance_method(method)
      else
        VI::UNDEF
      end
    end

    def inspect
      "{Module #{@name}}"
    end
  end
end
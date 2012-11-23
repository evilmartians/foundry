module Foundry
  class VMModule < VMObject
    attr_reader :name
    attr_reader :upperclass

    attr_reader :const_table, :method_table
    protected   :const_table, :method_table

    def initialize(klass, name=nil)
      super(klass)

      @name         = name.freeze
      @upperclass   = nil
      @const_table  = {}
      @method_table = {}
    end

    def include(modulus)
      @upperclass = VI::Foundry_IncludedModule.allocate(modulus, @upperclass)
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

    def const_defined?(name, search_parent=true)
      name = name.to_sym

      if exists = @const_table.key?(name)
        exists
      elsif search_parent && @upperclass
        @upperclass.const_defined? name
      else
        false
      end
    end

    def const_set(name, value)
      name = name.to_sym

      @const_table[name] = value
    end

    def const_get(name, search_parent=true)
      name = name.to_sym

      if value = @const_table[name]
        value
      elsif search_parent && @upperclass
        @upperclass.const_get name
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

    def method_defined?(name, search_parent=true)
      name = name.to_sym

      if @method_table.key?(name) &&
            !((undefined = @method_table[name]) == VI::UNDEF)
        true
      elsif !undefined && search_parent && @upperclass
        @upperclass.method_defined?(name)
      else
        false
      end
    end

    def define_method(name, value)
      name = name.to_sym

      @method_table[name] = value
    end

    def instance_method(name, search_parent=true)
      name = name.to_sym

      if value = @method_table[name]
        value
      elsif search_parent && @upperclass
        @upperclass.instance_method(name)
      else
        VI::UNDEF
      end
    end

    def inspect
      "{Module #{@name}}"
    end
  end
end
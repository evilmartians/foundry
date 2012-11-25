module Foundry
  class VMModule < VMObject
    attr_accessor :name
    attr_reader :upperclass

    define_mapped_ivars :name, :upperclass,
                        :const_table, :method_table

    attr_reader :const_table, :method_table
    protected   :const_table, :method_table

    def vm_initialize
      @name         = VI::NIL if defined?(VI::NIL)
      @upperclass   = VI::NIL if defined?(VI::NIL)
      @const_table  = {}
      @method_table = {}
    end

    def include(modulus)
      @upperclass = VI::Foundry_IncludedModule.vm_new(modulus, @upperclass)
    end

    def ancestors
      if @upperclass.nil?
        [ self ]
      else
        [ self ] + @upperclass.ancestors
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
      elsif search_parent && !@upperclass.nil?
        @upperclass.const_defined? name
      else
        false
      end
    end

    def const_set(name, value)
      name = name.to_sym

      if value.is_a?(VI::Module) && value.name.nil?
        # No VI.new_string defined yet.
        if self == VI::Object
          scope = ""
        else
          scope = "#{@name}::"
        end

        value.name = VI::String.vm_new("#{scope}#{name}")
      end

      @const_table[name] = value
    end

    def const_get(name, search_parent=true)
      name = name.to_sym

      if value = @const_table[name]
        value
      elsif search_parent && !@upperclass.nil?
        @upperclass.const_get name
      else
        VI::UNDEF
      end
    end

    def instance_methods(search_parent=true)
      if search_parent && !@upperclass.nil?
        (@method_table.keys + @upperclass.instance_methods).uniq
      else
        @method_table.keys
      end
    end

    def method_defined?(name, search_parent=true)
      name = name.to_sym

      if @method_table.key?(name) &&
            !(undefined = (@method_table[name] == VI::UNDEF))
        true
      elsif !undefined && search_parent && !@upperclass.nil?
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
      elsif search_parent && !@upperclass.nil?
        @upperclass.instance_method(name)
      else
        VI::UNDEF
      end
    end

    def inspect
      "{Module #{as_module_name @name, 'module'}}"
    end

    def as_module_name(name, entity)
      if name.nil?
        "<unnamed #{entity}>"
      else
        name.value
      end
    end
  end
end
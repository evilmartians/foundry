module Foundry
  class VMModule < VMObject
    attr_accessor :name
    attr_writer   :superclass
    attr_reader   :constant_table, :method_table

    define_mapped_ivars :name, :superclass,
                        :constant_table, :method_table

    def vm_initialize
      # Parts of initialization happen early enough that VI::NIL
      # is not defined. The name is overridden later anyway, and
      # host `nil` responds to #nil? just as well.
      # @superclass for BasicObject is fixed as well.
      if defined?(VI::NIL)
        @name         = VI::NIL
        @superclass   = VI::NIL
      end

      @constant_table = VMLookupTable.new
      @method_table   = VMLookupTable.new
    end

    def direct_superclass
      @superclass
    end

    def ancestors
      if @superclass.nil?
        [ self ]
      else
        [ self ] + @superclass.ancestors
      end
    end

    def constants(search_parent=true)
      if !@superclass.nil?
        (@constant_table.keys + @superclass.constants).uniq
      else
        @constant_table.keys
      end
    end

    def const_defined?(name, search_parent=true)
      name = name.to_sym

      if exists = @constant_table.key?(name)
        exists
      elsif search_parent && !@superclass.nil?
        @superclass.const_defined? name
      else
        false
      end
    end

    def const_set(name, value)
      name = name.to_sym

      if value.is_a?(VI::Module) && value.name.nil?
        if self == VI::Object
          scope = ""
        else
          scope = "#{@name}::"
        end

        # No VI.new_string defined yet.
        value.name = VI::String.vm_new("#{scope}#{name}")
      end

      @constant_table[name] = value
    end

    def const_get(name, search_parent=true)
      name = name.to_sym

      if @constant_table.key? name
        @constant_table[name]
      elsif search_parent && !@superclass.nil?
        @superclass.const_get name
      else
        VI::UNDEF
      end
    end

    def instance_methods(search_parent=true)
      if search_parent && !@superclass.nil?
        (@method_table.keys + @superclass.instance_methods).uniq
      else
        @method_table.keys
      end
    end

    def method_defined?(name, search_parent=true)
      name = name.to_sym

      if @method_table.key?(name) &&
            !(undefined = (@method_table[name] == VI::UNDEF))
        true
      elsif !undefined && search_parent && !@superclass.nil?
        @superclass.method_defined?(name)
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

      if @method_table.key? name
        @method_table[name]
      elsif search_parent && !@superclass.nil?
        @superclass.instance_method(name)
      else
        VI::UNDEF
      end
    end

    def inspect
      "{module #{as_module_name @name, 'module'}}"
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
module Registers
  module DSL
    def register(Integer offset, Symbol name, options={})
      size  = options[:size]  || 4
      align = options[:align] || size

      native_type = Integer.reify(width: size * 8)
      reg_class   = RegisterClass.reify(type: native_type, alignment: align)

      klass = Class.new(reg_class)
      const_set :"Reg#{name}", klass

      yield RegisterDefinition.new(klass)

      define_method(name) do |*fields|
        reg_ptr_class = reg_class.reify(address: @@base + offset)

        if fields.any?
          reg_ptr_class.new.fetch(fields)
        else
          reg_ptr_class.new
        end
      end

      define_method(:"#{name}=") do |new_value|
        reg_ptr_class = reg_class.reify(address: @@base + offset)

        if param.is_a?(Foundry::LookupTable)
          reg_ptr_class.new.update(param)
        else
          reg_ptr_class.new(param).store
        end
      end
    end
  end
end

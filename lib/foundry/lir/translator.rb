module Foundry
  class LIR::Translator
    attr_reader   :lir_module
    attr_reader   :llvm_module

    def initialize(name='foundry-generated-code')
      @lir_module  = LIR::Module.new
      @llvm_module = LLVM::Module.new(name)

      @methods     = {}
      @functions   = {}
    end

    def each_function(&block)
      @lir_module.each(&block)
    end

    def has_method?(proc)
      hash_key = proc.code

      @methods.key? hash_key
    end

    def add_method(proc)
      hash_key = proc.code

      if @methods.key? hash_key
        @methods[hash_key]
      else
        transform = LIR::Transform::FromHIR.new(@lir_module)

        binding   = LIR::Constant.new(proc.binding.type, proc.binding)
        function  = transform.run(proc.code, binding, proc.code.function)

        @methods[hash_key] = function
      end
    end

    def specialize(source_function, argument_types)
      hash_key = [source_function.name, argument_types]

      if @functions.key? hash_key
        @functions[hash_key]
      elsif source_function.arguments.map(&:type) == argument_types
        @functions[hash_key] = source_function
      else
        function = source_function.dup
        function.arguments.each_with_index do |arg, index|
          arg.type = argument_types[index]
        end
        @lir_module.add function

        hash_key = [function.name, argument_types]
        @functions[hash_key] = function
      end
    end
  end
end
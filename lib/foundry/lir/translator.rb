module Foundry
  class LIR::Translator
    attr_reader   :lir_module
    attr_reader   :llvm_module

    def initialize
      @lir_module  = LIR::Module.new

      @llvm_module = LLVM::Module.new('foundry-code')

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

        binding   = LIR::Constant.new(Foundry.typeof(proc.binding), proc.binding)
        function  = transform.run(proc.code, binding, proc.code.function)

        @methods[hash_key] = function
      end
    end

    def specialize(source_function, *types)
      key = [source_function.original_name, types]

      if @functions.key? key
        @functions[key]
      else
        function = source_function.dup

        @lir_module.add function
        @functions[key] = function

        yield function

        function
      end
    end
  end
end
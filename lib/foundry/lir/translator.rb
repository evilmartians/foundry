module Foundry
  class LIR::Translator
    attr_reader   :lir_module
    attr_reader   :llvm_module

    def initialize(name='foundry-generated-code')
      @lir_module  = LIR::Module.new
      @llvm_module = LLVM::Module.new(name)

      @cache       = {}
    end

    def each_function(&block)
      @lir_module.each(&block)
    end

    def has_method?(proc)
      hash_key = proc.code

      @cache.key? hash_key
    end

    def seed_method(proc)
      hash_key = proc.code

      if @cache.key? hash_key
        @cache[hash_key]
      else
        transform = LIR::Transform::FromHIR.new(@lir_module)
        function  = transform.transform(
              proc.code, proc.binding.to_static_env,
              proc.code.function, proc.binding)

        @cache[hash_key] = function
      end
    end
  end
end
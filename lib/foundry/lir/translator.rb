module Foundry
  class LIR::Translator
    attr_reader :lir_module
    attr_reader :llvm_module

    def initialize(name='foundry-generated-code')
      @lir_module  = LIR::Module.new
      @llvm_module = LLVM::Module.new(name)

      @cache       = {}
    end

    def run(proc, name)
      hash_key = proc.code

      if @cache.key? hash_key
        @cache[hash_key]
      else
        transform = LIR::Transform::FromHIR.new(@lir_module)
        function  = transform.transform(
              proc.code, [ proc.binding.to_set ], name)

        @cache[hash_key] = function.name
      end
    end
  end
end
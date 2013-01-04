module Foundry
  class LIR::Translator
    attr_reader   :lir_module
    attr_reader   :llvm_module

    attr_accessor :graph_lir

    def initialize(name='foundry-generated-code')
      @graph_lir   = false

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

        if @graph_lir
          puts "#{builder.function.pretty_print}\n"
        end

        @cache[hash_key] = function.name
      end
    end
  end
end
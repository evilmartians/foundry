module Foundry
  class LIR::Translator
    attr_reader   :lir_module
    attr_reader   :llvm_module

    def initialize
      @lir_module  = LIR::Module.new

      @llvm_module = LLVM::Module.new('foundry-code')

      # HACK
      @llvm_module.triple = 'x86_64-none'
      @llvm_module.data_layout = 'e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128'

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
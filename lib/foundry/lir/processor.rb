module Foundry
  class LIR::Processor
    def run(translator)
      translator.each_function.reduce(false) do |updated, func|
        if func.instrumentation
          transform_name = self.class.name.split('::').last
          func.instrumentation.transform_start(transform_name)
        end

        updated || run_on_function(translator, func)
      end
    end
  end
end
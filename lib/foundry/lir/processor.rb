module Foundry
  class LIR::Processor
    def run(translator)
      translator.each_function.reduce(false) do |updated, func|
        updated || run_on_function(translator, func)
      end
    end
  end
end
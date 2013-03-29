module Foundry
  class LIR::Processor
    def run(translator)
      translator.each_function.reduce(false) do |updated, func|
        instrumentation_trace(func)

        updated || begin
          run_on_function(translator, func)
        rescue => e
          $stderr.puts "Failure while processing function:"
          $stderr.puts func.awesome_print
          raise
        end
      end
    end

    def instrumentation_trace(func)
      if Furnace::SSA.instrumentation
        transform_name = self.class.name.split('::').last
        Furnace::SSA.instrumentation.mark_transform(func, transform_name)
      end
    end
  end
end

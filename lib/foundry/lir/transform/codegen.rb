module Foundry
  class LIR::Transform::Codegen
    def transform(translator)
      translator.lir_module.each do |lir_function|
        p lir_function.name
      end

      translator
    end
  end
end
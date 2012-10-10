module Foundry
  module Primitives
    @modules = []

    class << self
      attr_reader :modules
    end

    def self.included(modulus)
      @modules << modulus
    end

    def self.eval(name, interp, scope, args)
      method = :"eval_#{name}"

      Primitives.modules.each do |modulus|
        if modulus.respond_to? method
          return modulus.send method, interp, scope, args
        end
      end

      raise NotImplementedError, "unknown primitive #{name}"
    end

    def self.translate(name, node)
      raise NotImplementedError
    end
  end
end
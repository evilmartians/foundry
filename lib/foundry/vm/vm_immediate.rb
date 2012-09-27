module Foundry
  class VMImmediate < ::BasicObject
    def is_a?(klass)
      self.class.ancestors.include? klass
    end

    def respond_to?(method)
      self.class.method_defined? method
    end

    def method(method)
      self.class.instance_method(method)
    end

    def nil?
      false
    end
  end
end
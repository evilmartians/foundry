module Foundry::Interpreter
  class Context
    def initialize
      @functions = {}
      @next_id   = 0
    end

    def make_id
      @next_id += 1
    end

    def get(id)
      @functions[id]
    end

    def set(id, function)
      @functions[id] = function
    end
  end
end
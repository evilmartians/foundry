module Foundry
  class BlockEnvironment < VMImmediate
    attr_reader :scope
    attr_reader :executable

    attr_accessor :proc_environment

    def initialize(scope, executable, proc_environment=false)
      @scope      = scope
      @executable = executable
      @proc_environment = proc_environment
    end
  end
end
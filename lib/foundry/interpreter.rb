module Foundry::Interpreter
  VI = Foundry::VI
end

require_relative 'interpreter/backtrace_item'
require_relative 'interpreter/vm_error'
require_relative 'interpreter/context'

require_relative 'interpreter/base'
require_relative 'interpreter/ruby'
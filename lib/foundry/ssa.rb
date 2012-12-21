module Foundry::SSA
  include Furnace::SSA

  require_relative 'ssa/transform'

  require_relative 'ssa/instructions/binding'
  require_relative 'ssa/instructions/load'
  require_relative 'ssa/instructions/store'
  require_relative 'ssa/instructions/tuple'
  require_relative 'ssa/instructions/send'
  require_relative 'ssa/instructions/if'
end
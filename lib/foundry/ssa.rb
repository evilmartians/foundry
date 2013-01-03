module Foundry::SSA
  include Furnace::SSA

  require_relative 'ssa/builder'

  require_relative 'ssa/transform'

  require_relative 'ssa/instructions/binding'
  require_relative 'ssa/instructions/lvar_load'
  require_relative 'ssa/instructions/lvar_store'
  require_relative 'ssa/instructions/lambda'

  require_relative 'ssa/instructions/tuple'
  require_relative 'ssa/instructions/tuple_ref'
  require_relative 'ssa/instructions/tuple_bigger'
  require_relative 'ssa/instructions/tuple_slice'

  require_relative 'ssa/instructions/ivar_load'
  require_relative 'ssa/instructions/ivar_store'

  require_relative 'ssa/instructions/resolve'
  require_relative 'ssa/instructions/call'

  require_relative 'ssa/instructions/branch_if'

  require_relative 'ssa/instructions/define_method'
  require_relative 'ssa/instructions/define_module'
  require_relative 'ssa/instructions/define_class'

  require_relative 'ssa/instructions/check_arity'
  require_relative 'ssa/instructions/check_block'
end
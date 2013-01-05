require 'llvm/core'
require 'llvm/transforms/scalar'

module Foundry::LIR
  include Furnace::SSA

  require_relative 'lir/builder'

  require_relative 'lir/instructions/binding'
  require_relative 'lir/instructions/lvar_load'
  require_relative 'lir/instructions/lvar_store'
  require_relative 'lir/instructions/lambda'

  require_relative 'lir/instructions/tuple'
  require_relative 'lir/instructions/tuple_ref'
  require_relative 'lir/instructions/tuple_bigger'
  require_relative 'lir/instructions/tuple_slice'

  require_relative 'lir/instructions/ivar_load'
  require_relative 'lir/instructions/ivar_store'

  require_relative 'lir/instructions/invoke_method'
  require_relative 'lir/instructions/invoke_closure'

  require_relative 'lir/instructions/branch_if'

  require_relative 'lir/instructions/define_method'

  require_relative 'lir/instructions/check_arity'
  require_relative 'lir/instructions/check_block'

  require_relative 'lir/translator'

  module Transform
    require_relative 'lir/transform/from_hir'

    require_relative 'lir/transform/sparse_conditional_constant_propagation'
    require_relative 'lir/transform/codegen'
  end
end
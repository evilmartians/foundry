require 'llvm/core'
require 'llvm/transforms/scalar'

module Foundry::LIR
  include Furnace::SSA

  require_relative 'lir/analysis_error'

  require_relative 'lir/builder'

  require_relative 'lir/instructions/binding'
  require_relative 'lir/instructions/closure'
  require_relative 'lir/instructions/lvar_load'
  require_relative 'lir/instructions/lvar_store'

  require_relative 'lir/instructions/tuple'
  require_relative 'lir/instructions/tuple_ref'
  require_relative 'lir/instructions/tuple_bigger'
  require_relative 'lir/instructions/tuple_slice'

  require_relative 'lir/instructions/ivar_load'
  require_relative 'lir/instructions/ivar_store'

  require_relative 'lir/instructions/resolve_method'
  require_relative 'lir/instructions/resolve_closure'
  require_relative 'lir/instructions/invoke'

  require_relative 'lir/instructions/branch_if'

  require_relative 'lir/instructions/define_method'

  require_relative 'lir/instructions/integer_op'
  require_relative 'lir/instructions/trace'

  require_relative 'lir/instructions/check_arity'
  require_relative 'lir/instructions/check_block'

  require_relative 'lir/translator'
  require_relative 'lir/processor'

  module Transform
    require_relative 'lir/transform/from_hir'

    require_relative 'lir/transform/resolve_methods'
    require_relative 'lir/transform/specialize_methods'
    require_relative 'lir/transform/dead_code_elimination'
    require_relative 'lir/transform/global_dead_code_elimination'
    require_relative 'lir/transform/local_type_inference'
    require_relative 'lir/transform/sparse_conditional_constant_propagation'
    require_relative 'lir/transform/codegen'
  end
end
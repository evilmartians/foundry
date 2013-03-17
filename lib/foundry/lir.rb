require 'ffi' # warbler requires it to be here.

require 'llvm/core'
require 'llvm/analysis'
require 'llvm/transforms/scalar'

module Foundry
  def self.constant(value)
    LIR::Constant.new(Type.of(value), value)
  end
end

module Foundry::LIR
  include Furnace::SSA

  require 'foundry/lir/analysis_error'

  require 'foundry/lir/builder'

  require 'foundry/lir/instructions/allocate'

  require 'foundry/lir/instructions/binding'
  require 'foundry/lir/instructions/closure'
  require 'foundry/lir/instructions/lvar_load'
  require 'foundry/lir/instructions/lvar_store'

  require 'foundry/lir/instructions/tuple'
  require 'foundry/lir/instructions/tuple_ref'
  require 'foundry/lir/instructions/tuple_bigger'
  require 'foundry/lir/instructions/tuple_concat'
  require 'foundry/lir/instructions/tuple_slice'

  require 'foundry/lir/instructions/lut'

  require 'foundry/lir/instructions/const_ref'
  require 'foundry/lir/instructions/const_fetch'

  require 'foundry/lir/instructions/ivar_load'
  require 'foundry/lir/instructions/ivar_store'

  require 'foundry/lir/instructions/reify'

  require 'foundry/lir/instructions/resolve_method'
  require 'foundry/lir/instructions/resolve_closure'
  require 'foundry/lir/instructions/invoke'

  require 'foundry/lir/instructions/branch_if'

  require 'foundry/lir/instructions/define_method'

  require 'foundry/lir/instructions/integer_op'

  require 'foundry/lir/instructions/trace'

  require 'foundry/lir/instructions/check_arity'
  require 'foundry/lir/instructions/check_block'
  require 'foundry/lir/instructions/check_type'

  require 'foundry/lir/translator'
  require 'foundry/lir/processor'

  module Transform
    require 'foundry/lir/transform/from_hir'

    require 'foundry/lir/transform/resolve_methods'
    require 'foundry/lir/transform/specialize_methods'
    require 'foundry/lir/transform/dead_code_elimination'
    require 'foundry/lir/transform/global_dead_code_elimination'
    require 'foundry/lir/transform/local_type_inference'
    require 'foundry/lir/transform/return_type_inference'
    require 'foundry/lir/transform/binding_simplification'
    require 'foundry/lir/transform/basic_block_merging'
    require 'foundry/lir/transform/sparse_conditional_constant_propagation'
    require 'foundry/lir/transform/codegen'
  end
end

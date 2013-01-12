require 'ffi' # warbler requires it to be here.
require 'llvm/core'
require 'llvm/analysis'
require 'llvm/transforms/scalar'

module Foundry
  def self.typeof(object)
    if object.__vm_object?
      if object.singleton_class_defined?
        klass = object.singleton_class
      else
        klass = object.class
      end

      if klass == VI::Tuple
        LiteralTupleType.new(object.to_a)
      elsif klass == VI::Binding
        object.__type__ # TODO refactor this somehow
      elsif klass == VI::Proc
        ClosureType.new(klass)
      else
        Monotype.of(klass)
      end
    else
      case object
      when LIR::Value
        object.type
      else
        raise ArgumentError, "typeof(#{object.class}) is not defined"
      end
    end
  end

  def self.constant(value)
    LIR::Constant.new(typeof(value), value)
  end
end

module Foundry::LIR
  include Furnace::SSA

  def self.void
    Furnace::SSA.void
  end

  def self.void_value
    Furnace::SSA.void_value
  end

  require 'foundry/types/monotype'
  require 'foundry/types/tuple'
  require 'foundry/types/literal_tuple'
  require 'foundry/types/combined_tuple'
  require 'foundry/types/binding'
  require 'foundry/types/closure'

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

  require 'foundry/lir/instructions/const_ref'
  require 'foundry/lir/instructions/const_fetch'

  require 'foundry/lir/instructions/ivar_load'
  require 'foundry/lir/instructions/ivar_store'

  require 'foundry/lir/instructions/resolve_method'
  require 'foundry/lir/instructions/resolve_closure'
  require 'foundry/lir/instructions/invoke'

  require 'foundry/lir/instructions/branch_if'

  require 'foundry/lir/instructions/define_method'

  require 'foundry/lir/instructions/integer_op'

  require 'foundry/lir/instructions/trace'

  require 'foundry/lir/instructions/check_arity'
  require 'foundry/lir/instructions/check_block'

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
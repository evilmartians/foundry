require 'llvm/core'
require 'llvm/transforms/scalar'

module Foundry
  def self.typeof(object)
    if object.__vm_object?
      klass = object.class

      if klass == VI::Tuple
        TupleType.new(object.to_a)
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
end

module Foundry::LIR
  include Furnace::SSA

  def self.void
    Furnace::SSA.void
  end

  def self.void_value
    Furnace::SSA.void_value
  end

  require_relative 'types/monotype'
  require_relative 'types/tuple'
  require_relative 'types/binding'
  require_relative 'types/closure'

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
    require_relative 'lir/transform/return_type_inference'
    require_relative 'lir/transform/binding_simplification'
    require_relative 'lir/transform/sparse_conditional_constant_propagation'
    require_relative 'lir/transform/codegen'
  end
end
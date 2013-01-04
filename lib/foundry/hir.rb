module Foundry::HIR
  require_relative 'hir/node'
  require_relative 'hir/sexp_builder'
  require_relative 'hir/processor'

  module Transform
    require_relative 'hir/transform/from_ruby_parser'
    require_relative 'hir/transform/literal_primitives'
    require_relative 'hir/transform/argument_processing'
    require_relative 'hir/transform/trace_local_variables'
    require_relative 'hir/transform/expand_global_variables'
    require_relative 'hir/transform/dump_ir'
  end
end
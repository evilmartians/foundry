module Foundry::HIR
  require 'foundry/hir/node'
  require 'foundry/hir/sexp_builder'

  require 'foundry/hir/context'
  require 'foundry/hir/processor'

  module Transform
    require 'foundry/hir/transform/from_ruby_parser'
    require 'foundry/hir/transform/literal_primitives'
    require 'foundry/hir/transform/argument_processing'
    require 'foundry/hir/transform/trace_local_variables'
    require 'foundry/hir/transform/expand_global_variables'
    require 'foundry/hir/transform/dump_ir'
  end
end
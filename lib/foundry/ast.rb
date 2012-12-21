module Foundry::AST
  require_relative 'ast/node'
  require_relative 'ast/sexp_builder'
  require_relative 'ast/processor'
end

module Foundry::Transform
  require_relative 'transform/ruby_parser'
  require_relative 'transform/literal_primitives'
  require_relative 'transform/argument_processing'
  require_relative 'transform/trace_local_variables'
  require_relative 'transform/expand_global_variables'
  require_relative 'transform/dump_ir'
end
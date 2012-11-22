module Foundry::AST
end

require_relative 'ast/node'
require_relative 'ast/processor'

require_relative 'ast/prepare/ruby_parser'
require_relative 'ast/prepare/expand_primitives'
require_relative 'ast/prepare/expand_implicit_contexts'
require_relative 'ast/prepare/trace_variables'
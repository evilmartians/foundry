module Foundry
  class HIR::Transform::FromRubyParser < HIR::Processor
    require 'foundry/hir/transform/ruby_parser/literals'
    require 'foundry/hir/transform/ruby_parser/constants'
    require 'foundry/hir/transform/ruby_parser/interpolation'
    require 'foundry/hir/transform/ruby_parser/calls'
    require 'foundry/hir/transform/ruby_parser/formal_parameters'
    require 'foundry/hir/transform/ruby_parser/context_reification'
    require 'foundry/hir/transform/ruby_parser/global_variables'

    def initialize(is_eval)
      @is_eval = is_eval
    end

    def transform(ir)
      unless ir.type == :block
        ir = s(:block, ir)
      end

      process_toplevel_block(ir)
    end

    include Literals
    include Constants
    include Interpolation
    include Calls
    include FormalParameters
    include ContextReification
    include GlobalVariables
  end
end
module Foundry
  class HIR::Transform::FromRubyParser < HIR::Processor
    require_relative 'ruby_parser/literals'
    require_relative 'ruby_parser/constants'
    require_relative 'ruby_parser/interpolation'
    require_relative 'ruby_parser/calls'
    require_relative 'ruby_parser/formal_parameters'
    require_relative 'ruby_parser/context_reification'
    require_relative 'ruby_parser/global_variables'

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
module Foundry
  class Transform::RubyParser < AST::Processor
    require_relative 'ruby_parser/literals'
    require_relative 'ruby_parser/constants'
    require_relative 'ruby_parser/calls'
    require_relative 'ruby_parser/formal_parameters'
    require_relative 'ruby_parser/context_reification'

    def initialize(is_eval)
      @is_eval = is_eval
    end

    def transform(ast)
      unless ast.type == :block
        ast = s(:block, ast)
      end

      process_toplevel_block(ast)
    end

    include Literals
    include Constants
    include Calls
    include FormalParameters
    include ContextReification
  end
end
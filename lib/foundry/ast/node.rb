require 'furnace/ast'

module Foundry
  class AST::Node < Furnace::AST::Node
    attr_reader :line

    def self.from_sexp(node)
      (line, ), type, *children = node

      AST::Node.new(type,
        children.map do |child|
          if child.is_a? Array
            from_sexp(child)
          else
            child
          end
        end,
        line: line)
    end

    def self.from_sexp_without_location(node)
      type, *children = node

      AST::Node.new(type,
        children.map do |child|
          if child.is_a? Array
            from_sexp_without_location(child)
          else
            child
          end
        end)
    end
  end
end
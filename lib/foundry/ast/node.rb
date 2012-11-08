module Foundry
  class AST::Node < Furnace::AST::Node
    attr_reader :file, :line

    def self.from_sexp(node)
      type, *children = node

      AST::Node.new(type,
        children.map do |child|
          if child.is_a? Sexp
            from_sexp(child)
          else
            child
          end
        end,
        file: node.file.freeze,
        line: node.line)
    end

    # Used in tests.
    def self.from_simple_sexp(node)
      type, *children = node

      AST::Node.new(type,
        children.map do |child|
          if child.is_a? Array
            from_simple_sexp(child)
          else
            child
          end
        end)
    end
  end
end
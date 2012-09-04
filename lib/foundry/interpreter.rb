require 'pp'

module Foundry
  class InterpreterError < StandardError
  end

  class Interpreter
    AST = Melbourne::AST

    def initialize(method, scope)
      @method = method
      @scope  = scope
    end

    def evaluate
      visit @method.ast
    end

    def visit(node)
      case node
      when AST::Nil
        VM::NIL
      when AST::True
        VM::TRUE
      when AST::False
        VM::FALSE

      when AST::ConstFind
        process_const_find(node.name)

      when AST::Block
        process_block(node.array)
      when AST::Class
        pp node
        process_class(node.name, visit(node.superclass), node.body)

      else
        raise ::Exception, "unknown node #{node.class} on #{node.pretty_inspect}"
      end
    end

    def process_const_find(name)

    end

    def process_block(code)
      result = VM::NIL
      code.each do |statement|
        result = visit statement
      end
      result
    end

    def process_class(name, superclass, body)

    end

    private

    def const_get_polyfill(names, module_=VM::Object)
      next_part = names.shift

      if VM::Object.method_defined?(:const_get)
        raise "n/i"
      else
        if VM::Object.const_defined?(next_part)
          value = VM::Object.const_get(next_part)
        else
          raise InterpreterError, "bootstrap: unknown constant #{name}"
        end
      end
    end
  end
end
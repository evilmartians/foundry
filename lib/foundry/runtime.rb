module Foundry
  class Runtime
    class << self
      attr_accessor :interpreter
    end

    attr_reader   :toplevel

    attr_accessor :graph_ast
    attr_accessor :graph_ir

    def initialize
      @graph_ast = false
      @graph_ir  = true
      @toplevel  = Foundry::VI::Object.allocate
    end

    VM_ROOT = File.expand_path('../../../vm/', __FILE__)

    def bootstrap(vm_root)
      load_package(File.join(VM_ROOT, 'common'))
    end

    def load(filename)
      eval_ast Melbourne::Parser19.parse_file(filename), filename, create_toplevel_scope
    end

    def eval(string, name, scope=create_toplevel_scope)
      eval_ast Melbourne::Parser19.parse_string(string, name), name, scope
    end

    def create_toplevel_scope
      const_scope = ConstantScope.new([ VI::Object ])
      scope = VariableScope.new(@toplevel, VI::Object, nil, const_scope, [], nil)
      scope.function = '(toplevel)'
      scope
    end

    protected

    def pipeline
      Furnace::Transform::Pipeline.new([
        AST::Prepare::Melbourne.new,
        AST::Prepare::ExpandPrimitives.new,
      ])
    end

    def eval_ast(melbourne_ast, file, scope)
      ast = AST::Node.from_sexp(melbourne_ast.to_sexp)
      p ast if @graph_ast

      ir = pipeline.run(ast)
      p ir if @graph_ir

      script = ScriptBody.new(ir, file)
      script.execute(nil, scope)
    end

    def load_package(directory)
      package = File.read(File.join(directory, 'load_order.txt'))

      package.lines.each do |entry|
        puts "Loading #{entry}"

        entry_path = File.join(directory, entry.rstrip)
        if File.directory?(entry_path)
          load_package(entry_path)
        else
          load(entry_path)
        end
      end
    end
  end
end
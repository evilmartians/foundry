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

    def bootstrap
      load_package(File.join(VM_ROOT, 'common'))
    end

    def load_package(directory)
      package = File.read(File.join(directory, 'load_order.txt'))

      package.lines.each do |entry|
        entry_path = File.join(directory, entry.rstrip)
        if File.directory?(entry_path)
          load_package(entry_path)
        else
          load(entry_path)
        end
      end
    end

    def load(filename)
      ir = prepare_ast(parse_file(filename))
      eval_ir ir, filename, create_toplevel_scope
    end

    def eval(string, name='(eval)', scope=create_toplevel_scope)
      ir = prepare_ast(parse_string(string, name, scope))
      eval_ir ir, name, scope
    end

    def create_toplevel_scope
      const_scope = ConstantScope.new([ VI::Object ])
      scope = VariableScope.new(@toplevel, VI::Object, nil, const_scope, [], nil)
      scope.function = '(toplevel)'
      scope
    end

    def default_pipeline
      Furnace::Transform::Pipeline.new([
        AST::Prepare::RubyParser.new,
        AST::Prepare::ExpandPrimitives.new,
      ])
    end

    def parse_file(filename)
      parser = Ruby19Parser.new
      parser.parse(File.read(filename), filename)
    end

    def parse_string(string, name='(eval)', scope=nil)
      parser = Ruby19Parser.new
      if scope
        scope.locals.each do |name, |
          parser.env[name] = :lvar
        end
      end

      parser.parse(string, name)
    end

    def prepare_ast(input, pipeline=default_pipeline)
      ast = AST::Node.from_sexp(input)
      p ast if @graph_ast

      ir = pipeline.run(ast)
      p ir if @graph_ir

      ir
    end

    def eval_ir(ir, name='(unknown script)', scope=create_toplevel_scope)
      script = ScriptBody.new(ir, name)
      script.execute(nil, scope)
    end
  end
end
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
      #load_package(File.join(VM_ROOT, 'common'))
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
      parser = Ruby19Parser.new

      ir = prepare_ast(parser.parse(File.read(filename), filename))

      Runtime.interpreter.
          new(ir, @toplevel).
          evaluate
    end

    def eval(string, name='(eval)', outer=nil)
      parser = Ruby19Parser.new

      if outer
        locals = []
        outer.env.each do |name|
          locals << name
          parser.env[name] = :lvar
        end

        pipeline = make_pipeline(true, locals)
      else
        pipeline = make_pipeline(false)
      end

      ir = prepare_ast(parser.parse(string, name), pipeline)

      if outer
        Runtime.interpreter.
            new(ir, nil, nil, nil, outer.env, outer).
            evaluate
      else
        Runtime.interpreter.
            new(ir, @toplevel).
            evaluate
      end
    end

    def make_pipeline(is_eval=false, locals=nil)
      Furnace::Transform::Pipeline.new([
        AST::Prepare::RubyParser.new(is_eval),
        AST::Prepare::ExpandPrimitives.new,
        AST::Prepare::ExpandImplicitContexts.new,
        AST::Prepare::TraceVariables.new(locals),
      ])
    end

    def prepare_ast(input, pipeline=make_pipeline)
      ast = AST::Node.from_sexp(input)
      p ast if @graph_ast

      ir = pipeline.run(ast)
      p ir if @graph_ir

      ir
    end
  end
end
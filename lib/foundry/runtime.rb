module Foundry
  class Runtime
    class << self
      attr_accessor :interpreter

      attr_accessor :graph_ast
      attr_accessor :graph_ir
    end

    def self.initialize
      @graph_ast = false
      @graph_ir  = false
    end

    VM_ROOT = File.expand_path('../../../vm/', __FILE__)

    def self.bootstrap
      load_package(File.join(VM_ROOT, 'common'))
    end

    def self.load_package(directory)
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

    def self.load(filename)
      parser = Ruby19Parser.new

      ast = parser.parse(File.read(filename), filename)
      return if ast.nil?

      ir  = prepare_ast(ast)

      Runtime.interpreter.
          new(ir, VI::TOPLEVEL).
          evaluate
    end

    def self.eval(string, name='(eval)', outer=nil)
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
        @interpreter.
            new(ir, nil, nil, nil, outer.env, outer).
            evaluate
      else
        @interpreter.
            new(ir, VI::TOPLEVEL).
            evaluate
      end
    end

    def self.make_pipeline(is_eval=false, locals=nil)
      Furnace::Transform::Pipeline.new([
        AST::Prepare::RubyParser.new(is_eval),
        AST::Prepare::ExpandImplicitContexts.new,
        AST::Prepare::ExpandArgumentParsing.new,
        AST::Prepare::TraceVariables.new(locals),
        AST::Prepare::ExpandPrimitives.new,
      ])
    end

    def self.prepare_ast(input, pipeline=make_pipeline)
      ast = AST::Node.from_sexp(input)
      p ast if @graph_ast

      ir = pipeline.run(ast)
      p ir if @graph_ir

      ir
    end
  end
end
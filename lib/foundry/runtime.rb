module Foundry
  class Runtime
    class << self
      attr_accessor :interpreter

      attr_accessor :graph_ast
      attr_accessor :graph_ir
    end

    @graph_ast = false
    @graph_ir  = false

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
      eval_at_toplevel(File.read(filename), filename)
    end

    def self.eval(string, name='(eval)', outer=nil)
      if outer.nil?
        eval_at_toplevel(string, name)
      else
        eval_with_binding(string, name, outer)
      end
    end

    def self.eval_at_toplevel(source, name)
      parser = Ruby19Parser.new

      ir   = prepare_ast(parser.parse(source, name))
      proc = VI.new_proc(ir, nil)

      @interpreter.
          new(proc, VI::TOPLEVEL).
          evaluate
    end

    def self.eval_with_binding(source, name, outer)
      parser = Ruby19Parser.new

      locals = []
      outer.binding.each do |name|
        locals << name
        parser.env[name] = :lvar
      end

      pipeline = make_pipeline(true, locals)

      ir   = prepare_ast(parser.parse(source, name), pipeline)
      proc = VI.new_proc(ir, outer.binding)

      @interpreter.
          new(proc, nil, nil, nil, outer).
          evaluate
    end

    def self.make_pipeline(is_eval=false, locals=nil)
      Furnace::Transform::Pipeline.new([
        Transform::RubyParser.new(is_eval),
        Transform::ArgumentProcessing.new,
        Transform::TraceLocalVariables.new(locals),
        Transform::ExpandGlobalVariables.new,
        Transform::LiteralPrimitives.new,
        Transform::DumpIR.new,
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
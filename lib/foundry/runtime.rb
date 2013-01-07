module Foundry
  class Runtime
    class << self
      attr_accessor :interpreter

      attr_accessor :graph_ast
      attr_accessor :graph_hir
      attr_accessor :graph_lir
    end

    @graph_ast = false
    @graph_hir = false
    @graph_lir = true

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

      ir   = ast_to_hir(parser.parse(source, name))
      proc = VI.new_proc(ir, nil)

      @interpreter.
          new(proc, VI::TOPLEVEL).
          evaluate
    end

    def self.eval_with_binding(source, name, outer)
      parser = Ruby19Parser.new

      locals = Set[]
      outer.binding.each do |name|
        locals.add name
        parser.env[name] = :lvar
      end

      ir   = ast_to_hir(parser.parse(source, name), true, locals)
      proc = VI.new_proc(ir, outer.binding)

      @interpreter.
          new(proc, nil, nil, nil, outer).
          evaluate
    end

    def self.ast_to_hir(input, is_eval=false, locals=nil)
      pipeline = Furnace::Transform::Pipeline.new([
        HIR::Transform::FromRubyParser.new(is_eval),
        HIR::Transform::ArgumentProcessing.new,
        HIR::Transform::TraceLocalVariables.new(locals),
        HIR::Transform::ExpandGlobalVariables.new,
        HIR::Transform::LiteralPrimitives.new,
        HIR::Transform::DumpIR.new,
      ])

      ast = HIR::Node.from_sexp(input)
      p ast if @graph_ast

      hir_context = HIR::Context.new(ast)
      pipeline.run(hir_context)

      hir = hir_context.root

      p hir if @graph_hir

      hir
    end

    def self.compile
      pipeline = Furnace::Transform::Pipeline.new([
        Furnace::Transform::Iterative.new([
          LIR::Transform::ResolveMethods.new,
          LIR::Transform::SpecializeMethods.new,
          LIR::Transform::LocalTypeInference.new,
          LIR::Transform::SparseConditionalConstantPropagation.new,
          LIR::Transform::DeadCodeElimination.new,
        ]),

        LIR::Transform::GlobalDeadCodeElimination.new([ 'main' ]),
        LIR::Transform::Codegen.new,
      ])

      translator = LIR::Translator.new

      toplevel = construct_toplevel_call('main')
      translator.lir_module.add toplevel

      pipeline.run(translator)

      if @graph_lir
        translator.each_function do |func|
          puts "#{func.pretty_print}\n"
        end
      end

      translator
    end

    def self.construct_toplevel_call(name)
      builder = LIR::Builder.new(name, [], LIR.void)

      toplevel = builder.toplevel

      method = builder.resolve_method [
            toplevel,
            builder.symbol(name)
          ]

      builder.invoke nil,
          [ method,
            toplevel,
            builder.tuple,
            builder.nil
          ]

      builder.return LIR.void_value

      builder.function
    end
  end
end
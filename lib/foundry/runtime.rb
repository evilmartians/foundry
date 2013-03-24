require 'benchmark'
require 'llvm/linker'
require 'llvm/transforms/builder'

LLVM::Target.init_all(true)

module Foundry
  class Runtime
    class << self
      attr_accessor :interpreter

      attr_accessor :graph_ast
      attr_accessor :graph_hir
      attr_accessor :graph_lir
      attr_accessor :instrument
    end

    @graph_ast  = false
    @graph_hir  = false
    @graph_lir  = false
    @instrument = false

    VM_ROOT  = File.expand_path('../../../vm/',  __FILE__)
    RTL_ROOT = File.expand_path('../../../rtl/', __FILE__)

    def self.bootstrap
      $stderr.puts "Bootstrapping VM..."

      time = Benchmark.realtime do
        %w(common baremetal stm32/f1).each do |package|
          load_package(File.join(VM_ROOT, package))
        end
      end

      $stderr.puts "VM loading complete in %d ms." % [time * 1000]
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

    def self.optimize(translator)
      pipeline = Furnace::Transform::Pipeline.new([
        Furnace::Transform::Iterative.new([
          LIR::Transform::ResolveMethods.new,
          LIR::Transform::SpecializeMethods.new,
          LIR::Transform::LocalTypeInference.new,
          LIR::Transform::ReturnTypeInference.new,
          LIR::Transform::BindingSimplification.new,
          LIR::Transform::SparseConditionalConstantPropagation.new,
          LIR::Transform::DeadCodeElimination.new,
          LIR::Transform::BasicBlockMerging.new,
          LIR::Transform::Inline.new,
        ], debug: true),

        LIR::Transform::GlobalDeadCodeElimination.new([ 'main' ]),
      ])

      toplevel = construct_toplevel_call('main')
      translator.lir_module.add toplevel

      time = Benchmark.realtime do
        pipeline.run(translator)
      end

      $stderr.puts "Optimization complete in %d ms." % [time * 1000]

      if @graph_lir
        translator.each_function do |func|
          puts "#{func.awesome_print}\n"
        end
      end

      translator
    end

    def self.compile(translator)
      transform = LIR::Transform::Codegen.new
      transform.run(translator)

      if @graph_llvm
        translator.llvm_module.dump
      end

      translator
    end

    def self.link(translator)
      linked_mod = LLVM::Module.new('foundry-linked')

      # Link the translated module.
      translator.llvm_module.link_into linked_mod

      # Link the runtime library.
      rtl = LLVM::Module.parse_bitcode(File.join(RTL_ROOT, 'testbench.bc'))
      rtl.link_into_and_destroy(linked_mod)

      target = LLVM::Target.by_name('x86-64')
      unless target.target_machine?
        raise ArgumentError, "Target #{target.name} does not have a TargetMachine available"
      end

      machine = target.create_machine('x86_64-linux-gnu')

      builder = LLVM::PassManagerBuilder.new
      builder.opt_level          = 3     # -O3
      builder.size_level         = 1     # -Os
      builder.simplify_lib_calls = false
      builder.inliner_threshold  = 225   # -O2

      pass_manager = LLVM::PassManager.new(machine)
      builder.build_with_lto(pass_manager, true, true)

      linked_mod.triple      = machine.triple
      linked_mod.data_layout = machine.data_layout

      pass_manager.run(linked_mod)

      [ machine, linked_mod ]
    end

    def self.construct_toplevel_call(name)
      builder = LIR::Builder.new(name, [], Type.bottom, instrument: @instrument)

      toplevel = builder.toplevel

      method = builder.resolve_method \
          [ toplevel, builder.symbol(name) ]

      retv = builder.invoke Type.top,
          [ method, toplevel, builder.tuple, builder.nil ]

      builder.return

      builder.function
    end
  end
end

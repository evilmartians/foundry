module Foundry
  class Runtime
    attr_accessor :graph_ast

    def initialize
      @graph_ast = false
      @toplevel  = Foundry::VI::Object.allocate
    end

    def bootstrap(vm_root)
      load_package(File.join(vm_root, 'base'))
    end

    def load(filename)
      eval_ast Melbourne::Parser19.parse_file(filename), create_toplevel_scope
    end

    def eval(string, name, scope=create_toplevel_scope)
      eval_ast Melbourne::Parser19.parse_string(string, name), scope
    end

    def create_toplevel_scope
      const_scope = Foundry::ConstantScope.new([ Foundry::VI::Object ])
      Foundry::VariableScope.new(@toplevel, Foundry::VI::Object, nil, const_scope, [], nil)
    end

    protected

    def eval_ast(ast, scope)
      if @graph_ast
        ast.ascii_graph
      end

      script = Foundry::ScriptBody.new(ast)
      script.execute(scope)
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
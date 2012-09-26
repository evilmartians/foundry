module Foundry
  class VMImmediate < ::BasicObject
    def nil?
      false
    end

    def vm_nil?
      false
    end

    def vm_true?
      true
    end
  end

  class ConstantScope < VMImmediate
    attr_reader :nesting

    def initialize(nesting)
      @nesting = VI::Tuple.allocate(nesting.to_ary)
    end

    def find_const(name)
      @nesting.to_a.find { |klass| klass.const_get(name) }
    end

    def nest(klass)
      ConstantScope.new([ klass ] + @nesting)
    end
  end

  class VariableScope < VMImmediate
    attr_reader :method
    attr_reader :module
    attr_reader :parent
    attr_reader :const_scope
    attr_reader :arguments
    attr_reader :block

    attr_reader :locals

    def initialize(method, module_, parent, const_scope, arguments, block)
      @method, @module, @parent, @const_scope = method, module_, parent, const_scope
      @arguments, @block = VI::Tuple.allocate(arguments.to_ary), block
      @locals = {}
    end
  end

  class BlockEnvironment < VMImmediate
    attr_reader :scope
    attr_reader :code

    attr_accessor :proc_environment

    def initialize(scope, code, proc_environment=false)
      @scope     = scope
      @code      = code
      @proc_environment = proc_environment
    end
  end

  class Executable < VMImmediate
    attr_reader :ast

    def initialize(ast)
      @ast = ast
    end

    def execute(scope)
      interp = Interpreter.new(scope)
      interp.evaluate
    end
  end

  class Script < Executable
  end

  class Method < Executable
    attr_reader :parameters

    def initialize(ast, parameters)
      super(ast)
      @parameters = parameters
    end

    def call(arguments, block)
      raise
    end
  end

  class VMObject < VMImmediate
    attr_reader :class

    def initialize(klass)
      @class      = klass
      @ivar_table = {}
    end

    def is_a?(klass)
      @class.ancestors.include
    end

    def instance_variables
      @ivar_table.keys
    end

    def instance_variable_set(ivar, value)
      @ivar_table[ivar] = value
    end

    def instance_variable_get(ivar)
      @ivar_table[ivar]
    end
  end

  class VMModule < VMObject
    attr_reader :name
    attr_reader :upperclass

    def initialize(klass, name=nil)
      super(klass)

      @name         = name
      @upperclass   = nil
      @const_table  = {}
      @method_table = {}
    end

    def include(module_)
      @upperclass = VMIncludedModule.new(module_, @upperclass)
    end

    def ancestors
      [ self ] + @upperclass.ancestors
    end

    def constants(search_parent=true)
      if @upperclass
        (@const_table.keys + @upperclass.constants).uniq
      else
        @const_table.keys
      end
    end

    def const_defined?(const, search_parent=true)
      if exists = @const_table.key?(const)
        exists
      elsif search_parent && @upperclass
        @upperclass.const_defined? const
      else
        false
      end
    end

    def const_set(const, value)
      @const_table[const] = value
    end

    def const_get(const, search_parent=true)
      if value = @const_table[const]
        value
      elsif search_parent && @upperclass
        @upperclass.const_get const
      else
        false
      end
    end

    def instance_methods(search_parent=true)
      if @upperclass
        (@method_table.keys + @upperclass.instance_methods).uniq
      else
        @method_table.keys
      end
    end

    def method_defined?(method, search_parent=true)
      if exists = @method_table.key?(method)
        exists
      elsif search_parent && @upperclass
        @upperclass.method_defined?(method)
      else
        false
      end
    end

    def define_method(method, value)
      @method_table[method] = value
    end

    def instance_method(method, search_parent=true)
      if value = @method_table[method]
        value
      elsif search_parent && @upperclass
        @upperclass.instance_method(method)
      else
        false
      end
    end
  end

  class VMIncludedModule < VMModule
    attr_reader :module
    attr_reader :upperclass

    def initialize(klass, module_, upperclass)
      super(klass)

      @module       = module_
      @upperclass   = upperclass
      @const_table  = module_.const_table
      @method_table = module_.method_table
    end

    def ancestors
      [ @module ] + @upperclass.ancestors
    end
  end

  class VMClass < VMModule
    attr_reader :superclass

    def initialize(klass, superclass, name=nil, vm_class=VMObject)
      super(klass, name)

      @vm_class   = vm_class
      @superclass = superclass
    end

    def allocate(*args)
      unless @vm_class.ancestors.include? VMObject
        ::Kernel.send :raise, ::Exception, "cannot allocate VMClass instance for VMImmediate"
      else
        @vm_class.new(self, *args)
      end
    end
  end

  class VMTuple < VMObject
    def initialize(klass, value)
      @storage = value.to_ary.freeze
    end

    def [](index)
      @storage[index.to_int]
    end

    def length
      @storage.length
    end

    def each(&block)
      @storage.each &block
    end

    def to_a
      @storage
    end
    alias to_ary to_a
  end

  class VMNilClass < VMImmediate
    def class
      VM::NilClass
    end

    def vm_nil?
      true
    end

    def vm_true?
      false
    end
  end

  class VMTrueClass < VMImmediate
    def class
      VM::TrueClass
    end
  end

  class VMFalseClass < VMImmediate
    def class
      VM::FalseClass
    end

    def vm_true?
      false
    end
  end

  class VMSymbol < VMImmediate
    attr_reader :value

    def initialize(klass, value)
      @value = value.to_sym
    end

    def class
      VM::Symbol
    end
  end

  class VMString < VMObject
    attr_reader :value

    def initialize(klass, value)
      super(klass)

      @value = value.to_str
    end
  end

  class VMNumeric < VMObject
  end

  class VMInteger < VMNumeric
    attr_reader :value

    def initialize(klass, value)
      super(klass)

      @value = value.to_int
    end
  end

  module VI
    BasicObject = VMClass.new(nil, nil,         :BasicObject, VMObject)
    Object      = VMClass.new(nil, BasicObject, :Object,      VMObject)
    Module      = VMClass.new(nil, Object,      :Module,      VMModule)
    Class       = VMClass.new(nil, Module,      :Class,       VMClass)

    [BasicObject, Object, Module, Class].each do |klass|
      klass.instance_exec do
        @class = Class
      end
    end

    NilClass    = Class.allocate(Object, :NilClass,   VMNilClass)
    TrueClass   = Class.allocate(Object, :TrueClass,  VMTrueClass)
    FalseClass  = Class.allocate(Object, :FalseClass, VMFalseClass)

    NIL         = VMNilClass.new
    TRUE        = VMTrueClass.new
    FALSE       = VMFalseClass.new

    Symbol      = Class.allocate(Object, :Symbol, VMSymbol)
    String      = Class.allocate(Object, :String, VMString)

    Numeric     = Class.allocate(Object, :Numeric,  VMNumeric)
    Integer     = Class.allocate(Integer, :Integer, VMInteger)

    Foundry     = Module.allocate(:Foundry)

    IncludedModule = Class.allocate(Module, :"Foundry::IncludedModule", VMIncludedModule)
    Tuple          = Class.allocate(Object, :"Foundry::Tuple", VMTuple)

    ConstantScope    = Class.allocate(Object, :"Foundry::ConstantScope", ConstantScope)
    VariableScope    = Class.allocate(Object, :"Foundry::VariableScope", VariableScope)
    BlockEnvironment = Class.allocate(Object, :"Foundry::BlockEnvironment", BlockEnvironment)
    Executable       = Class.allocate(Object, :"Foundry::Executable", Executable)
    Script           = Class.allocate(Executable, :"Foundry::Script", Script)
    Method           = Class.allocate(Executable, :"Foundry::Method", Method)

    BasicObject.const_set :BasicObject, BasicObject

    Object.const_set :Object, Object
    Object.const_set :Module, Module
    Object.const_set :Class,  Class

    Object.const_set :NilClass,   NilClass
    Object.const_set :TrueClass,  TrueClass
    Object.const_set :FalseClass, FalseClass

    Object.const_set :Symbol,  Symbol
    Object.const_set :String,  String

    Object.const_set :Integer, Integer
    Object.const_set :Numeric, Numeric

    Object.const_set :Foundry, Foundry

    Foundry.const_set :IncludedModule, IncludedModule
    Foundry.const_set :Tuple,          Tuple

    Foundry.const_set :ConstantScope,    ConstantScope
    Foundry.const_set :VariableScope,    VariableScope
    Foundry.const_set :BlockEnvironment, BlockEnvironment
    Foundry.const_set :Executable,       Executable
    Foundry.const_set :Script,           Script
    Foundry.const_set :Method,           Method
  end
end
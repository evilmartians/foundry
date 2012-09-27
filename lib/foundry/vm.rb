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
      @nesting = nesting.to_a.dup.freeze
    end

    def find_const(name)
      @nesting.each do |klass|
        value = klass.const_get(name)
        return value if value
      end
    end

    def nest(klass)
      ConstantScope.new([ klass ] + @nesting)
    end
  end

  class VariableScope < VMImmediate
    attr_reader :self
    attr_reader :module
    attr_reader :parent
    attr_reader :const_scope
    attr_reader :arguments
    attr_reader :block

    attr_reader :locals

    def initialize(ourself, modulus, parent, const_scope, arguments, block)
      @self, @module, @parent, @const_scope = ourself, modulus, parent, const_scope
      @arguments, @block = arguments.to_a.dup.freeze, block
      @locals = ::Hash.new { VI::NIL }
    end
  end

  class BlockEnvironment < VMImmediate
    attr_reader :scope
    attr_reader :executable

    attr_accessor :proc_environment

    def initialize(scope, executable, proc_environment=false)
      @scope      = scope
      @executable = executable
      @proc_environment = proc_environment
    end
  end

  class Executable < VMImmediate
    attr_reader :ast

    def initialize(ast)
      @ast = ast
    end

    def execute(scope)
      interp = Interpreter.new(self, scope)
      interp.evaluate
    end
  end

  class ScriptBody < Executable
  end

  class MethodBody < Executable
    attr_reader :module
    attr_reader :parameters

    def initialize(ast, modulus, parameters, primitive)
      super(ast)
      @module     = modulus
      @parameters = parameters
      @primitive  = primitive
    end

    def execute(scope)
      if @primitive
        if value = @module.__send__(:"_#{@primitive}", scope)
          return value
        end
      end

      super
    end
  end

  class ClosureBody < Executable
  end

  class VMObject < VMImmediate
    attr_reader :class

    def initialize(klass)
      @class      = klass
      @ivar_table = {}
    end

    def is_a?(klass)
      @class.ancestors.include? klass
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

    def respond_to?(method)
      @class.method_defined? method
    end

    def method(method)
      @class.instance_method(method)
    end

    def inspect
      if @ivar_table.any?
        ivs = " "
        @ivar_table.each do |key, value|
          ivs << "@#{key}=#{value}.inspect"
        end
      end

      "{#{@class.name}:#{__id__}#{ivs}}"
    end
  end

  class VMModule < VMObject
    attr_reader :name
    attr_reader :upperclass

    attr_reader :const_table, :method_table
    protected   :const_table, :method_table

    def initialize(klass, name=nil)
      super(klass)

      @name         = name
      @upperclass   = nil
      @const_table  = {}
      @method_table = {}
    end

    def include(modulus)
      @upperclass = VI::Foundry_IncludedModule.allocate(modulus, @upperclass)
    end

    def _include(scope)
      scope.self.include(*scope.arguments)
    end

    def ancestors
      if @upperclass
        [ self ] + @upperclass.ancestors
      else
        [ self ]
      end
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

    def inspect
      "{Module #{@name}}"
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

    def inspect
      "{IncludedModule #{@name}}"
    end
  end

  class VMClass < VMModule
    attr_reader :superclass

    def initialize(klass, superclass, name=nil, vm_class=VMObject)
      super(klass, name)

      @vm_class   = vm_class
      @superclass = superclass
      @upperclass = superclass
    end

    def allocate(*args)
      unless @vm_class.ancestors.include? VMObject
        ::Kernel.send :raise, ::Exception, "cannot allocate VMClass instance for VMImmediate"
      else
        @vm_class.new(self, *args)
      end
    end

    def _allocate(scope)
      scope.self.allocate
    end

    def inspect
      "{Class #{@name} < #{@superclass.name}}"
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

    def inspect
      "{Tuple #{@storage.inspect}}"
    end
  end

  class VMNilClass < VMImmediate
    def class
      VI::NilClass
    end

    def vm_nil?
      true
    end

    def vm_true?
      false
    end

    def inspect
      "{nil}"
    end
  end

  class VMTrueClass < VMImmediate
    def class
      VI::TrueClass
    end

    def inspect
      "{true}"
    end
  end

  class VMFalseClass < VMImmediate
    def class
      VI::FalseClass
    end

    def vm_true?
      false
    end

    def inspect
      "{false}"
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

    def inspect
      "{:#{@value}}"
    end
  end

  class VMString < VMObject
    attr_reader :value

    def initialize(klass, value)
      super(klass)

      @value = value.to_str
    end

    def inspect
      "{#{@value.inspect}}"
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

    def inspect
      "{#{@value}}"
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

    Kernel      = Module.allocate(:Kernel)

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

    Foundry_IncludedModule   = Class.allocate(Module, :"Foundry::IncludedModule", VMIncludedModule)
    Foundry_Tuple            = Class.allocate(Object, :"Foundry::Tuple", VMTuple)

    Foundry_ConstantScope    = Class.allocate(Object, :"Foundry::ConstantScope", ConstantScope)
    Foundry_VariableScope    = Class.allocate(Object, :"Foundry::VariableScope", VariableScope)
    Foundry_BlockEnvironment = Class.allocate(Object, :"Foundry::BlockEnvironment", BlockEnvironment)

    Foundry_Executable       = Class.allocate(Object, :"Foundry::Executable", Executable)
    Foundry_ScriptBody       = Class.allocate(Executable, :"Foundry::ScriptBody", ScriptBody)
    Foundry_MethodBody       = Class.allocate(Executable, :"Foundry::MethodBody", MethodBody)
    Foundry_ClosureBody      = Class.allocate(Executable, :"Foundry::ClosureBody", ClosureBody)

    BasicObject.const_set :BasicObject, BasicObject

    Object.const_set :Object, Object
    Object.const_set :Module, Module
    Object.const_set :Class,  Class

    Object.const_set :Kernel, Kernel

    Object.const_set :NilClass,   NilClass
    Object.const_set :TrueClass,  TrueClass
    Object.const_set :FalseClass, FalseClass

    Object.const_set :NIL,   NIL
    Object.const_set :TRUE,  TRUE
    Object.const_set :FALSE, FALSE

    Object.const_set :Symbol,  Symbol
    Object.const_set :String,  String

    Object.const_set :Integer, Integer
    Object.const_set :Numeric, Numeric

    Object.const_set :Foundry, Foundry

    Foundry.const_set :IncludedModule,   Foundry_IncludedModule
    Foundry.const_set :Tuple,            Foundry_Tuple

    Foundry.const_set :ConstantScope,    Foundry_ConstantScope
    Foundry.const_set :VariableScope,    Foundry_VariableScope
    Foundry.const_set :BlockEnvironment, Foundry_BlockEnvironment
    Foundry.const_set :Executable,       Foundry_Executable
    Foundry.const_set :ScriptBody,       Foundry_ScriptBody
    Foundry.const_set :MethodBody,       Foundry_MethodBody
    Foundry.const_set :ClosureBody,      Foundry_ClosureBody
  end
end
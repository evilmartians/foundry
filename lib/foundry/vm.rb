require_relative 'vm/vm_immediate'
require_relative 'vm/vm_object'

require_relative 'vm/vm_module'
require_relative 'vm/vm_included_module'
require_relative 'vm/vm_class'

require_relative 'vm/vm_tuple'

require_relative 'vm/vm_nil_class'
require_relative 'vm/vm_true_class'
require_relative 'vm/vm_false_class'

require_relative 'vm/vm_binding'
require_relative 'vm/vm_proc'

require_relative 'vm/vm_symbol'
require_relative 'vm/vm_string'

require_relative 'vm/vm_integer'

module Foundry
  module VI
    BasicObject = VMClass.new(nil)
    BasicObject.vm_initialize(nil, 'BasicObject', VMObject)

    Object      = VMClass.new(nil)
    Object.vm_initialize(BasicObject, 'Object', VMObject)

    Module      = VMClass.new(nil)
    Module.vm_initialize(Object, 'Module', VMModule)

    Class       = VMClass.new(nil)
    Class.vm_initialize(Module, 'Class', VMClass)

    [BasicObject, Object, Module, Class].each do |klass|
      klass.instance_exec do
        @class = Class
      end
    end

    Kernel        = Module.vm_new('Kernel')

    NilClass      = Class.vm_new(Object, 'NilClass',   VMNilClass)
    TrueClass     = Class.vm_new(Object, 'TrueClass',  VMTrueClass)
    FalseClass    = Class.vm_new(Object, 'FalseClass', VMFalseClass)

    UNDEF         = :undefined
    NIL           = VMNilClass.new
    TRUE          = VMTrueClass.new
    FALSE         = VMFalseClass.new

    Binding       = Class.vm_new(Object, 'Binding', VMBinding)
    Proc          = Class.vm_new(Object, 'Proc', VMProc)

    Symbol        = Class.vm_new(Object, 'Symbol', VMSymbol)
    String        = Class.vm_new(Object, 'String', VMString)

    Numeric       = Class.vm_new(Object, 'Numeric',  VMObject)
    Integer       = Class.vm_new(Numeric, 'Integer', VMInteger)

    Foundry       = Module.vm_new('Foundry')

    Foundry_IncludedModule   = Class.vm_new(Module, 'Foundry::IncludedModule', VMIncludedModule)
    Foundry_SingletonClass   = Class.vm_new(Class,  'Foundry::SingletonClass', VMClass)
    Foundry_Tuple            = Class.vm_new(Object, 'Foundry::Tuple', VMTuple)

    BasicObject.const_set :BasicObject, BasicObject

    Object.const_set :Object, Object
    Object.const_set :Module, Module
    Object.const_set :Class,  Class

    Object.const_set :Kernel, Kernel

    Object.const_set :NilClass,   NilClass
    Object.const_set :TrueClass,  TrueClass
    Object.const_set :FalseClass, FalseClass

    Object.const_set :Binding, Binding
    Object.const_set :Proc,    Proc

    Object.const_set :Symbol,  Symbol
    Object.const_set :String,  String

    Object.const_set :Numeric, Numeric
    Object.const_set :Integer, Integer

    Object.const_set :Foundry, Foundry

    Foundry.const_set :IncludedModule, Foundry_IncludedModule
    Foundry.const_set :SingletonClass, Foundry_SingletonClass
    Foundry.const_set :Tuple,          Foundry_Tuple

    TOPLEVEL = Object.vm_new

    def self.new_module(name)
      Module.vm_new(name)
    end

    def self.new_class(superclass, name)
      Class.vm_new(superclass, name, VMObject)
    end

    def self.new_binding
      Binding.vm_new(nil)
    end

    def self.new_proc(binding, code)
      Proc.vm_new(binding, code)
    end

    def self.new_symbol(value)
      Symbol.vm_new(value)
    end

    def self.new_string(value)
      String.vm_new(value)
    end

    def self.new_integer(value)
      Integer.vm_new(value)
    end

    def self.new_tuple(value)
      Foundry_Tuple.vm_new(value)
    end
  end
end
require_relative 'vm/vm_immediate'
require_relative 'vm/vm_object'

require_relative 'vm/vm_module'
require_relative 'vm/vm_included_module'
require_relative 'vm/vm_class'
require_relative 'vm/vm_singleton_class'

require_relative 'vm/vm_tuple'
require_relative 'vm/tuple_type'

require_relative 'vm/vm_lookup_table'

require_relative 'vm/vm_nil_class'
require_relative 'vm/vm_true_class'
require_relative 'vm/vm_false_class'

require_relative 'vm/vm_binding'
require_relative 'vm/binding_type'

require_relative 'vm/vm_proc'

require_relative 'vm/vm_symbol'
require_relative 'vm/vm_string'

require_relative 'vm/vm_integer'

module Foundry
  module VI
    UNDEF         = :undefined
    NIL           = VMNilClass.new
    TRUE          = VMTrueClass.new
    FALSE         = VMFalseClass.new

    BasicObject   = VMClass.new(nil)
    BasicObject.vm_initialize(nil, VMObject)

    Object        = VMClass.new(nil)
    Object.vm_initialize(BasicObject, VMObject)

    Module        = VMClass.new(nil)
    Module.vm_initialize(Object, VMModule)

    Class         = VMClass.new(nil)
    Class.vm_initialize(Module, VMClass)

    [BasicObject, Object, Module, Class].each do |klass|
      klass.__send__ :initialize, Class
    end

    NilClass      = Class.vm_new(Object, VMNilClass)
    TrueClass     = Class.vm_new(Object, VMTrueClass)
    FalseClass    = Class.vm_new(Object, VMFalseClass)

    Kernel        = Module.vm_new

    Binding       = Class.vm_new(Object, VMBinding)
    Proc          = Class.vm_new(Object, VMProc)

    Symbol        = Class.vm_new(Object, VMSymbol)
    String        = Class.vm_new(Object, VMString)

    Numeric       = Class.vm_new(Object,  VMObject)
    Integer       = Class.vm_new(Numeric, VMInteger)

    Foundry       = Module.vm_new

    Foundry_IncludedModule   = Class.vm_new(Module, VMIncludedModule)
    Foundry_SingletonClass   = Class.vm_new(Class,  VMSingletonClass)
    Foundry_Tuple            = Class.vm_new(Object, VMTuple)
    Foundry_LookupTable      = Class.vm_new(Object, VMLookupTable)

    BasicObject.instance_exec do
      @name       = String.vm_new('BasicObject')
      @superclass = NIL
    end

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
    Foundry.const_set :LookupTable,    Foundry_LookupTable

    TOPLEVEL = Object.vm_new

    def self.new_module
      Module.vm_new
    end

    def self.new_class(superclass)
      Class.vm_new(superclass)
    end

    def self.new_binding
      Binding.vm_new(nil)
    end

    def self.new_proc(code, binding)
      Proc.vm_new(code, binding)
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
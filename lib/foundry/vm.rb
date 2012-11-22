require_relative 'vm/vm_immediate'

require_relative 'vm/vm_basic_object'
require_relative 'vm/vm_object'

require_relative 'vm/vm_module'
require_relative 'vm/vm_included_module'
require_relative 'vm/vm_class'

require_relative 'vm/vm_tuple'

require_relative 'vm/vm_nil_class'
require_relative 'vm/vm_true_class'
require_relative 'vm/vm_false_class'

require_relative 'vm/vm_symbol'
require_relative 'vm/vm_string'
require_relative 'vm/vm_numeric'
require_relative 'vm/vm_integer'

module Foundry
  module VI
    BasicObject = VMClass.new(nil, nil,         'BasicObject', VMBasicObject)
    Object      = VMClass.new(nil, BasicObject, 'Object',      VMObject)
    Module      = VMClass.new(nil, Object,      'Module',      VMModule)
    Class       = VMClass.new(nil, Module,      'Class',       VMClass)

    [BasicObject, Object, Module, Class].each do |klass|
      klass.instance_exec do
        @class = Class
      end
    end

    Kernel      = Module.allocate('Kernel')

    NilClass    = Class.allocate(Object, 'NilClass',   VMNilClass)
    TrueClass   = Class.allocate(Object, 'TrueClass',  VMTrueClass)
    FalseClass  = Class.allocate(Object, 'FalseClass', VMFalseClass)

    UNDEF       = :undefined
    NIL         = VMNilClass.new
    TRUE        = VMTrueClass.new
    FALSE       = VMFalseClass.new

    Symbol      = Class.allocate(Object, 'Symbol', VMSymbol)
    String      = Class.allocate(Object, 'String', VMString)

    Numeric     = Class.allocate(Object, 'Numeric',  VMNumeric)
    Integer     = Class.allocate(Numeric, 'Integer', VMInteger)

    Foundry     = Module.allocate('Foundry')

    Foundry_IncludedModule   = Class.allocate(Module, 'Foundry::IncludedModule', VMIncludedModule)
    Foundry_SingletonClass   = Class.allocate(Class,  'Foundry::SingletonClass', VMClass)
    Foundry_Tuple            = Class.allocate(Object, 'Foundry::Tuple', VMTuple)

    BasicObject.const_set :BasicObject, BasicObject

    Object.const_set :Object, Object
    Object.const_set :Module, Module
    Object.const_set :Class,  Class

    Object.const_set :Kernel, Kernel

    Object.const_set :NilClass,   NilClass
    Object.const_set :TrueClass,  TrueClass
    Object.const_set :FalseClass, FalseClass

    Object.const_set :Symbol,  Symbol
    Object.const_set :String,  String

    Object.const_set :Integer, Integer
    Object.const_set :Numeric, Numeric

    Object.const_set :Foundry, Foundry

    Foundry.const_set :IncludedModule,   Foundry_IncludedModule
    Foundry.const_set :SingletonClass,   Foundry_SingletonClass
    Foundry.const_set :Tuple,            Foundry_Tuple
  end
end
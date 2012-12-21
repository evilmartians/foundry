module Foundry::Globals
  @values  = Foundry::LookupTable.new
  @aliases = Foundry::LookupTable.new

  def self.define_alias(Symbol alias_to, Symbol name)
    @aliases[alias_to] = name
  end

  def self.resolve_alias(Symbol name)
    if @aliases.key? name
      @aliases[name]
    else
      name
    end
  end

  def self.get(Symbol name)
    @values[resolve_alias(name)]
  end

  def self.set(Symbol name, value)
    @values[resolve_alias(name)] = value
  end
end
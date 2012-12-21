module Foundry::Globals
  @values = Foundry::LookupTable.new

  def self.get(Symbol name)
    trace "$get", name
    @values[name]
  end

  def self.set(Symbol name, value)
    trace "$set", name, value
    @values[name] = value
  end
end
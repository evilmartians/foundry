def reload_vm!
  Foundry.send :remove_const, :VI
  Foundry::Interpreter.send :remove_const, :VI
  load File.expand_path('../../../lib/foundry/vm.rb', __FILE__)

  Foundry::Interpreter.const_set :VI, Foundry::VI

  $f = Foundry::Runtime.new
  $f.bootstrap
end
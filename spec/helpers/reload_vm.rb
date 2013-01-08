def reload_vm!
  Foundry.send :remove_const, :VI
  Foundry::Evaluator.send :remove_const, :VI
  load File.expand_path('../../../lib/foundry/vm.rb', __FILE__)

  Foundry::Evaluator.const_set :VI, Foundry::VI

  Foundry::Runtime.bootstrap
end
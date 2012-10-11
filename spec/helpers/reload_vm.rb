def reload_vm!
  Foundry.send :remove_const, :VI
  load File.expand_path('../../../lib/foundry/vm.rb', __FILE__)

  $f = Foundry::Runtime.new
  $f.bootstrap
end
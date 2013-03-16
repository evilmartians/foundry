# -*- encoding: utf-8 -*-
require File.expand_path('../lib/foundry/version', __FILE__)

Gem::Specification.new do |gem|
  gem.authors       = ["Peter Zotov"]
  gem.email         = ["whitequark@whitequark.org"]
  gem.description   = %q{Foundry is a hybrid Ruby interpreter/compiler which aims to } <<
                      %q{optimize performance and footprint for various targets.}
  gem.summary       = gem.description
  gem.homepage      = ""

  gem.files         = `git ls-files bin lib vm rtl README.md`.split($\)
  gem.executables   = gem.files.grep(%r{^bin/}).map{ |f| File.basename(f) }
  gem.test_files    = gem.files.grep(%r{^(test|spec|features)/})
  gem.name          = "foundry"
  gem.require_paths = ["lib"]
  gem.version       = Foundry::VERSION

  gem.add_dependency "ansi"
  gem.add_dependency "trollop"
  gem.add_dependency "furnace",               '= 0.4.0.beta.1'
  gem.add_dependency "foundry-ruby_parser",   '= 3.1.1'
  gem.add_dependency "ruby-llvm",             '= 3.2.0.beta.1'
  gem.add_dependency "json"

  gem.add_development_dependency "rake",         '~> 10.0'
  gem.add_development_dependency "rspec"
  gem.add_development_dependency "simplecov"
  gem.add_development_dependency "warbler"
  gem.add_development_dependency "furnace-xray", '~> 1.0'
end

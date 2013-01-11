#!/usr/bin/env rake

require 'rspec/core/rake_task'
require 'warbler'

TARGET_JRUBY_VERSION = "1.7.2"

RSpec::Core::RakeTask.new(:spec)
Warbler::Task.new

task :default => :spec

task :jar => "java/jruby-complete.jar"

desc "Download jruby-complete.jar"
file "java/jruby-complete.jar" do
  require 'open-uri'

  url = "http://repository.codehaus.org/org/jruby/jruby-complete/#{TARGET_JRUBY_VERSION}/jruby-complete-#{TARGET_JRUBY_VERSION}.jar"

  File.open('java/jruby-complete.jar', 'w') do |f|
    f.write open(url).read
  end
end

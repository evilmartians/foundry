#!/usr/bin/env rake

require 'rspec/core/rake_task'
require 'warbler'
require 'fileutils'
require 'open-uri'

### CONFIGURATION ###

TARGET_JRUBY_VERSION = "1.7.2"

DEBIAN_PACKAGES = {
  ['llvm-3.2'   , 'libllvm3.2'  , '3.2-2'                  ] =>
      { 'libLLVM-3.2.so.1' => 'libLLVM-3.2.so' },
  ['gcc-4.7'    , 'libstdc++6'  , '4.7.2-5'                ] => {},
  ['gcc-4.7'    , 'libgcc1'     , '4.7.2-5'                ] => {},
  ['eglibc'     , 'libc6'       , '2.16-0experimental1'    ] => {},
  ['libffi'     , 'libffi5'     , '3.0.10-3'               ] => {},
  ['glib2.0'    , 'libglib2.0-0', '2.33.12+really2.32.4-5' ] => {},
  ['zlib'       , 'zlib1g'      , '1.2.7.dfsg-13'          ] => {},
  ['pcre3'      , 'libpcre3'    , '8.31-2'                 ] => {},
  ['libselinux' , 'libselinux1' , '2.1.9-5'                ] => {},
}

DEBIAN_MIRROR = 'ru'

### END CONFIGURATION ###

task :default => :spec

RSpec::Core::RakeTask.new(:spec)
Warbler::Task.new

def download(url, file)
  puts "Downloading #{url}"

  mkdir_p File.dirname(file)
  File.open(file, 'w') do |f|
    f.write open(url).read
  end
end

task :jar => "redist/java/jruby-complete.jar"
file "foundry.jar" do
  raise "JRuby required" unless defined?(JRUBY_VERSION)
  Rake::Task['jar'].invoke
end

desc "Build a redistributable archive."
task :redist => "foundry.jar" do
  rm_rf   "pkg"

  release_id = Time.now.strftime("%Y%M%d")
  target     = "pkg/foundry-#{release_id}"

  mkdir_p target

  cp_r "redist/native",     target
  cp   "foundry.jar",       target
  cp   "redist/foundry.sh", target

  %w(vm rtl).each do |lib|
    cp_r lib, target
  end

  Bundler.definition.specs_for([ :default ]).each do |spec|
    next if %w(ffi foundry).include? spec.name

    Dir[spec.full_gem_path + "**/*"].
          grep(/(LICENSE|COPYING|README)/).
          each do |file|
      mkdir_p "#{target}/legal/#{spec.name}"
      cp file, "#{target}/legal/#{spec.name}/"
    end
  end

  sh "cd pkg; tar czvf foundry-#{release_id}.tgz #{File.basename(target)}"
end

desc "Commit to the redist repository."
task "redist:lock" do
  sh "cd redist; git add -A; git commit -m 'Update dependencies'"
  sh "git add redist"
end

desc "Rebuild all dependencies for redistributables."
task "redist:deps" => %w(redist/java/jruby-complete.jar redist:linux)

file "redist/java/jruby-complete.jar" do
  download("http://repository.codehaus.org/org/jruby/jruby-complete/#{TARGET_JRUBY_VERSION}/jruby-complete-#{TARGET_JRUBY_VERSION}.jar", 'redist/java/jruby-complete.jar')
end

desc "Download and unpack binary dependencies from Debian packages"
task "redist:linux" => "foundry.jar" do
  puts "Gathering memory maps"

  mkdir_p "tmp"
  sh "java -jar foundry.jar --print-maps tmp/map"

  shlibs = File.readlines('tmp/map').map do |line|
    _, _, _, _, _, soname = line.split
    if soname &&
        soname.start_with?('/') &&
        soname.include?('.so') &&
        soname !~ /libnss_/
      File.basename(soname)
    end
  end.compact.uniq

  %w(i386 amd64).each do |src_arch|
    target_arch = { 'i386' => 'i386', 'amd64' => 'x86_64' }[src_arch]
    native_path = "redist/native/linux-#{target_arch}"

    rm_rf native_path
    mkdir_p native_path

    DEBIAN_PACKAGES.each do |(src_pkg, lib_pkg, version), symlinks|
      prefix = src_pkg.match(/(lib)?./)[0]
      url = "http://ftp.#{DEBIAN_MIRROR}.debian.org/debian/pool/main/#{prefix}/#{src_pkg}/#{lib_pkg}_#{version}_#{src_arch}.deb"

      rm_rf "tmp"
      mkdir_p "tmp"

      download(url, 'tmp/package.deb')

      # data.tar.gz or data.tar.xz
      sh "cd tmp; ar x package.deb; tar xf data.tar.*"

      found_any = []

      Dir["tmp/{usr/,}lib/#{target_arch}-linux-gnu/**/*.so*"].each do |lib|
        soname = lib
        soname = File.readlink(soname) if File.symlink?(soname)
        soname = File.basename(soname)

        next unless shlibs.include? soname

        target = "#{native_path}/#{File.basename(lib)}"

        if File.symlink?(lib)
          ln_s File.readlink(lib), target
        else
          cp lib, target
        end

        found_any = true
      end

      unless found_any
        raise "Cannot find any used files in package #{lib_pkg}-#{version}"
      end

      symlinks.each do |from, to|
        ln_s from, "#{native_path}/#{to}"
      end
    end
  end
end
require 'spec_helper'

describe "reload_vm! helper" do
  it "should flush constants" do
    reload_vm!
    $f.eval('A = 10')
    expect { $f.eval('A') }.not_to raise_error
    reload_vm!
    expect { $f.eval('A') }.to raise_error
  end
end
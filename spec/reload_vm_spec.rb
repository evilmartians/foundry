require 'spec_helper'

describe "reload_vm! helper" do
  it "should flush constants" do
    reload_vm!
    Foundry::Runtime.eval('A = 10')
    expect { Foundry::Runtime.eval('A') }.not_to raise_error
    reload_vm!
    expect { Foundry::Runtime.eval('A') }.to raise_error
  end
end
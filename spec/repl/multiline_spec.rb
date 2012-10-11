require 'spec_helper'

describe REPL::Multiline do
  it "should report single-line statement as complete" do
    REPL::Multiline.multiline?('class A; end').should be_false
  end

  it "should report incomplete multi-line statement as incomplete" do
    REPL::Multiline.multiline?('class A').should be_true
  end

  it "should report complete multi-line statement as complete" do
    REPL::Multiline.multiline?("class A\nend").should be_false
  end
end
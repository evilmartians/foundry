require 'spec_helper'

describe HIR::Node do
  it "should convert from sexp with location" do
    tree = HIR::Node.from_sexp(RubyParser.new.parse("1 + \n2", '(chunky bacon)'))

    tree.line.should == 1
    tree.file.should == '(chunky bacon)'
    receiver, name, arg = tree.children
    receiver.line.should == 1
    arg.line.should == 2
  end

  it "should convert from sexp without location" do
    HIR::Node.from_simple_sexp([ :node, [ :nested, 42 ] ]).should ==
        HIR::Node.new(:node, [ HIR::Node.new(:nested, [ 42 ]) ])
  end
end
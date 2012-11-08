require 'spec_helper'

describe "AST::Node" do
  it "should convert from sexp with location" do
    tree = AST::Node.from_sexp(RubyParser.new.parse("1 + \n2", '(chunky bacon)'))

    tree.line.should == 1
    tree.file.should == '(chunky bacon)'
    receiver, name, arg = tree.children
    receiver.line.should == 1
    arg.line.should == 2
  end

  it "should convert from sexp without location" do
    AST::Node.from_simple_sexp([ :node, [ :nested, 42 ] ]).should ==
        AST::Node.new(:node, [ AST::Node.new(:nested, [ 42 ]) ])
  end
end
require 'spec_helper'

describe "AST::Node" do
  it "should convert from sexp with location" do
    nested = AST::Node.new(:nested, [42], line: 2)
    tree = AST::Node.new(:node, [ nested ], line: 1)

    AST::Node.from_sexp([ [1], :node, [ [2], :nested, 42 ] ]).should == tree
  end

  it "should convert from sexp without location" do
    AST::Node.from_sexp_without_location([ :node, [ :nested, 42 ] ]).should ==
        AST::Node.new(:node, [ AST::Node.new(:nested, [ 42 ]) ])
  end

  it "should ignore location in comparison" do
    AST::Node.from_sexp([ [1], :node, [ [2], :nested, 42 ] ]).should ==
        s[:node, [:nested, 42]]
  end
end
require 'spec_helper'

describe "AST transformations" do
  class CountingTransform < AST::Transform
    attr_reader :count

    def initialize
      @count = 0
    end

    def on_integer(node)
      value, = node.children
      @count += 1 if value == 42

      nil
    end

    def on_symbol(node)
      value, = node.children
      @count += 1 if value == :marvin

      nil
    end

    def on_const(node)
      name, value = node.children
      @count += 1 if name == :HHGG

      nil
    end
  end

  def process(pipeline, string)
    ir = $f.prepare_ast($f.parse_string(string), pipeline)
  end

  it "should visit nodes with nested nodes" do
    transform = CountingTransform.new
    pipeline  = Furnace::Transform::Pipeline.new([
      AST::Prepare::RubyParser.new,
      AST::Prepare::ExpandPrimitives.new,
      transform,
    ])

    lambda {
      process(pipeline, 'A = 42')
    }.should change(transform, :count).by(1)

    lambda {
      process(pipeline, %Q|class A < HHGG; 42; end|)
    }.should change(transform, :count).by(2)

    lambda {
      process(pipeline, %Q|module A; 42; end|)
    }.should change(transform, :count).by(1)

    lambda {
      # "x = 42" shound _not_ be visited,
      # as args are interpreted in a special way
      process(pipeline, %Q|def f(x = 42); 42; end|)
    }.should change(transform, :count).by(1)

    lambda {
      process(pipeline, %Q|42.marvin(42, 42)|)
    }.should change(transform, :count).by(3)

    lambda {
      process(pipeline, %Q|a = 42|)
    }.should change(transform, :count).by(1)

    lambda {
      process(pipeline, %Q|0 + 42; 1 + 42|)
    }.should change(transform, :count).by(2)

    lambda {
      process(pipeline, %Q|alias marvin marvin; alias :marvin :marvin|)
    }.should change(transform, :count).by(4)
  end

  it "expands primitives" do
    pipeline  = Furnace::Transform::Pipeline.new([
      AST::Prepare::RubyParser.new,
      AST::Prepare::ExpandPrimitives.new,
    ])

    process(pipeline, %Q|Foundry.primitive :send, :foo, "bar"|).should ==
        s[:send, [:symbol, :foo], [:string, "bar"]]
  end
end
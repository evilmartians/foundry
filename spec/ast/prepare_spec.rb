require 'spec_helper'

describe "AST transformations" do
  def process(pipeline, string)
    ir = $f.prepare_ast($f.parse_string(string), pipeline)
  end

  it "should visit nodes with nested nodes" do
    transform = CountingTransform.new
    pipeline  = Furnace::Transform::Pipeline.new([
      AST::Prepare::Melbourne.new,
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
      process(pipeline, %Q|42; 42|)
    }.should change(transform, :count).by(2)

    lambda {
      process(pipeline, %Q|alias marvin marvin; alias :marvin :marvin|)
    }.should change(transform, :count).by(4)
  end

  it "expands primitives" do
  end
end
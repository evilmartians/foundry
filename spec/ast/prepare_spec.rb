require 'spec_helper'

describe "AST transformations" do
  def eval_with_pipeline(pipeline, string)
    ir = $f.prepare_ast($f.parse_string(string), pipeline)
  end

  let(:transform) {
    CountingTransform.new
  }

  let(:pipeline) {
    Furnace::Transform::Pipeline.new([
      AST::Prepare::Melbourne.new,
      AST::Prepare::ExpandPrimitives.new,
      transform,
    ])
  }

  it "should visit nodes with nested nodes" do
    lambda {
      eval_with_pipeline(pipeline, 'A = 42')
    }.should change(transform, :count).by(1)

    lambda {
      eval_with_pipeline(pipeline, %Q|class A < HHGG; 42; end|)
    }.should change(transform, :count).by(2)
  end
end
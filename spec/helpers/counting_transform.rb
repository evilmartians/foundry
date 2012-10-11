class CountingTransform < AST::Transform
  attr_reader :count

  def initialize
    @count = 0
  end

  def on_lit_integer(node)
    value, = node.children
    @count += 1 if value == 42

    nil
  end

  def on_const(node)
    name, value = node.children
    @count += 1 if name == :HHGG

    nil
  end
end
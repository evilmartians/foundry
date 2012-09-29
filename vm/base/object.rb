class Object < BasicObject
  include Kernel

  def nil?
    false
  end

  alias send __send__
end
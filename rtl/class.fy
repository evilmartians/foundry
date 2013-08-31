class Class
  def define_method(name, body)
    invokeprimitive cls_defm(self, name, body)
  end
end

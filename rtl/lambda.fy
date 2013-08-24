class Lambda
  def call(*args, **kwargs)
    invokeprimitive lam_call(self, args, kwargs)
  end
end

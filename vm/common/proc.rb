class Proc
  def self.allocate
    raise TypeError, "allocator undefined for Proc"
  end

  def call(*args, &block)
    FoundryRt.proc_call self, args, block
  end

  def binding
    @binding
  end

  def lambda_style!
    @lambda = true
  end

  def lambda?
    @lambda
  end
end
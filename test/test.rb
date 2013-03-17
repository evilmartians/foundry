def main
  tuple_magic
end

def tuple_magic
  tup = [ 1, 2 ]
  tup2 = [ *tup, "a", tup ]
end

def fact(n)
  if n > 1
    n * fact(n - 1)
  else
    1
  end
end

def fib(n) => Integer
  if n < 2
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

def closure
  5.times { trace 42 }
end

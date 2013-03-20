def main
  trace fact(10)
  trace fib(10)
end

def tuple_magic
  tup = [ 1, 2 ]
  tup2 = [ *tup, "a", tup ]
end

def fact(n) => Machine::S32
  if n > 1
    n * fact(n - 1)
  else
    1
  end
end

def fib(n) => Machine::S32
  if n < 2
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

def closure
  5.times { trace 42 }
end

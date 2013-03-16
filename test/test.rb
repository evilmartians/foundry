def main
  trace 1
  #trace fact(5)
  #trace fib(10)
end

def fact(n)
  if n > 1
    n * fact(n - 1)
  else
    1
  end
end

def fib(n)
  if n < 2
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

# trace 120

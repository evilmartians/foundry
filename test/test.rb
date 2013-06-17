class Board
  def self.setup
    # RCC->APB2ENR |= IOPCEN; // 0x10
    FoundryRt.store 0x40021000 + 0x18, 0x10, 4
    # GPIOC->CRH = 0x44444411;
    FoundryRt.store 0x40011000 + 0x04, 0x44444411, 4
  end

  def self.led_on=(value)
    what = value ? 0x100 : 0
    # GPIOC->ODR = BIT(8);
    FoundryRt.store 0x40011000 + 0x0C, what, 4
  end
end

def delay
  i = 0
  while i < 1000000
    i += 1
  end
end

def main
  Board.setup
  while true
    Board.led_on = true
    delay
    Board.led_on = false
    delay
  end
end

def loop
  i = 0.to_u8
  while i < 30.to_u8
    trace i
    i += 1.to_u8
  end
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

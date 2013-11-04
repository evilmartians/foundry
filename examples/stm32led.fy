Board = MapleLeaf.new

def delay(n) : (\a, Unsigned(32)) -> Nil
  n.times((i) do
    RCC.APB2ENR # volatile do nothing.
  end)

  nil
end

def main
  Board.setup

  while true
    Board.led_on = true
    self.delay(100000)
    Board.led_on = false
    self.delay(100000)
  end
end

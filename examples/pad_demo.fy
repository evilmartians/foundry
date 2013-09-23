Board = FoundryGamepad.new

def main
  Board.setup

  let mut x = 0u32
  let mut y = 0u32
  while true
    Board.set_pixel(x:, y:, true)
    if Board.up_pressed?
      y -= 1
    elsif Board.down_pressed?
      y += 1
    elsif Board.left_pressed?
      x -= 1
    elsif Board.right_pressed?
      x += 1
    end

    x &= 127
    y &= 63

    Board.delay(25_000)
  end
end

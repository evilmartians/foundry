Board = Gamepad.new
LCD   = Board.lcd

def main
  Board.setup

  let mut paddle_width = 12u32
  let mut paddle_pos   = 64 - paddle_width / 2

  while true
    if Board.left_pressed? && paddle_pos > 0
      paddle_pos -= 1
    elsif Board.right_pressed? && paddle_pos + paddle_width < 128
      paddle_pos += 1
    end

    LCD.clear
    # paddle
    LCD.rectangle(x: paddle_pos, y: 60, w: paddle_width, h: 2, true)

    LCD.refresh
    Board.delay(5_000)
  end
end

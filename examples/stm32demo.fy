Board = MapleLeaf.new
LCD   = ColorLCDShield.new

def main
  Board.setup_pll(16_000_000)
  LCD.setup
  LCD.clear(0xFF, 0xFF, 0xFF)
  LCD.backlight_on = true

  let mut cx = 10u8
  let mut cy = 10u8

  while true
    LCD.clip(x1: cx, y1: cy, x2: cx + 1, y2: cy + 1)

    LCD.start

    let mut i = 0u32

    while i < 2 * 2
      if LCD.left_pressed?
        LCD.put_pixel(0xFF, 0x00, 0x00)
      elsif LCD.top_pressed?
        LCD.put_pixel(0x00, 0xFF, 0x00)
      elsif LCD.right_pressed?
        LCD.put_pixel(0x00, 0x00, 0xFF)
      elsif LCD.center_pressed?
        LCD.put_pixel(0xFF, 0xFF, 0x00)
      elsif LCD.bottom_pressed?
        LCD.put_pixel(0xFF, 0x00, 0xFF)
      else
        LCD.put_pixel(0x00, 0x00, 0x00)
      end

      i += 1
    end

    LCD.finish

    if LCD.left_pressed?
      cx -= 1
    elsif LCD.top_pressed?
      cy -= 1
    elsif LCD.right_pressed?
      cx += 1
    elsif LCD.bottom_pressed?
      cy += 1
    end
    LCD.delay(50000)
  end
end

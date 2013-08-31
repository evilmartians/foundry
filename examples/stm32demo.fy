Board = MapleLeaf.new
LCD   = ColorLCDShield.new

def main
  Board.setup
  LCD.setup
  LCD.clear(0xFFu8, 0xFFu8, 0xFFu8)

  let mut cx = 10u8
  let mut cy = 10u8

  while true
    LCD.clip(x1: cx, y1: cy, x2: cx + 1u8, y2: cy + 1u8)

    LCD.start
    let mut i = 0u32
    while i < 2u32 * 2u32
      if LCD.left_pressed?
        LCD.put_pixel(0xFFu8, 0x00u8, 0x00u8)
      elsif LCD.top_pressed?
        LCD.put_pixel(0x00u8, 0xFFu8, 0x00u8)
      elsif LCD.right_pressed?
        LCD.put_pixel(0x00u8, 0x00u8, 0xFFu8)
      elsif LCD.center_pressed?
        LCD.put_pixel(0xFFu8, 0xFFu8, 0x00u8)
      elsif LCD.bottom_pressed?
        LCD.put_pixel(0xFFu8, 0x00u8, 0xFFu8)
      else
        LCD.put_pixel(0x00u8, 0x00u8, 0x00u8)
      end
      i += 1u32
    end
    LCD.finish

    if LCD.left_pressed?
      cx -= 1u8
    elsif LCD.top_pressed?
      cy -= 1u8
    elsif LCD.right_pressed?
      cx += 1u8
    elsif LCD.bottom_pressed?
      cy += 1u8
    end
    LCD.delay(50000u32)
  end
end

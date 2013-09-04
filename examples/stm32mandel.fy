Board = MapleLeaf.new
LCD   = ColorLCDShield.new

def main
  Board.setup
  LCD.setup
  LCD.backlight_on = true
  LCD.clear(0xFFu8, 0xFFu8, 0xFFu8)
  LCD.clip(x1: 0x02u8, y1: 0x02u8,
           x2: 0x81u8, y2: 0x81u8)

  let mut cx = -0x24ccccccc_s64
  let mut cy = -0x14ccccccc_s64
  let mut dx =   0x08ebbbbb_s64
  let mut dy =   0x08ebbbbb_s64

  let mandelconverger = (real, imag, iters, creal, cimag) do
    let real_sq = ((real >> 8s64) * (real >> 8s64)) >> 16s64
    let imag_sq = ((imag >> 8s64) * (imag >> 8s64)) >> 16s64
    let len_sq  = ((real >> 8s64) * (imag >> 8s64)) >> 16s64
    if iters > 200u8 || real_sq + imag_sq > 0x400000000s64
      iters
    else
      mandelconverger.call(real_sq - imag_sq + creal,
                           2s64 * len_sq + cimag,
                           iters + 1u8, creal, cimag)
    end
  end

  while true
    LCD.start

    let mut py = cy
    128u32.times((_) do
      let mut px = cx
      128u32.times((_) do
        let density = mandelconverger.call(px, py, 0u8, px, py)
        if density > 14u8
          LCD.put_pixel(0xFFu8 - (density - 14u8) * 36u8,
                        0x00u8 + (density - 14u8) * 36u8,
                        0x00u8)
        elsif density > 7u8
          LCD.put_pixel(0x00u8,
                        0xFFu8 - (density - 7u8) * 36u8,
                        0x00u8 + (density - 7u8) * 36u8)
        else
          LCD.put_pixel(0x00u8,
                        0x00u8,
                        0xFFu8 - density * 36u8)
        end

        px += dx
      end)
      py += dy
    end)

    LCD.finish

    let mut pressed = false
    while !pressed
      if LCD.left_pressed?
        cx -= 20s64 * dx
        pressed = true
      elsif LCD.top_pressed?
        cy -= 20s64 * dx
        pressed = true
      elsif LCD.right_pressed?
        cx += 20s64 * dy
        pressed = true
      elsif LCD.bottom_pressed?
        cy += 20s64 * dy
        pressed = true
      elsif LCD.center_pressed?
        let dx_old = dx
        let dy_old = dy
        dx = (dx * 0xB0000000s64) >> 32s64
        dy = (dy * 0xB0000000s64) >> 32s64
        cx += (dx_old - dx) * 64s64
        cy += (dy_old - dy) * 64s64
        pressed = true
      end
    end
  end
end

Board = MapleLeaf.new
LCD   = ColorLCDShield.new

def main
  Board.setup_pll(72_000_000)
  LCD.setup
  LCD.backlight_on = true
  LCD.clear(0xFF, 0xFF, 0xFF)
  LCD.clip(x1: 0x02, y1: 0x02, x2: 0x81, y2: 0x81)

  let mut cx = -0x24ccccccc_s64
  let mut cy = -0x14ccccccc_s64
  let mut dx =   0x08ebbbbb_s64
  let mut dy =   0x08ebbbbb_s64

  let mandelconverger = (real, imag, iters, creal, cimag) do
    let real_sq = ((real >> 8) * (real >> 8)) >> 16
    let imag_sq = ((imag >> 8) * (imag >> 8)) >> 16
    let len_sq  = ((real >> 8) * (imag >> 8)) >> 16
    if iters > 200 || real_sq + imag_sq > 0x400000000
      iters
    else
      mandelconverger.call(real_sq - imag_sq + creal,
                           2 * len_sq + cimag,
                           iters + 1, creal, cimag)
    end
  end

  while true
    LCD.start

    let mut py = cy
    128u32.times((_) do
      let mut px = cx
      128u32.times((_) do
        let density = mandelconverger.call(px, py, 0u8, px, py)
        if density > 14
          LCD.put_pixel(0xFF - (density - 14) * 36,
                        0x00 + (density - 14) * 36,
                        0x00)
        elsif density > 7
          LCD.put_pixel(0x00,
                        0xFF - (density - 7) * 36,
                        0x00 + (density - 7) * 36)
        else
          LCD.put_pixel(0x00,
                        0x00,
                        0xFF - density * 36)
        end

        px += dx
      end)
      py += dy
    end)

    LCD.finish

    let mut pressed = false
    while !pressed
      if LCD.left_pressed?
        cx -= 20 * dx
        pressed = true
      elsif LCD.top_pressed?
        cy -= 20 * dx
        pressed = true
      elsif LCD.right_pressed?
        cx += 20 * dy
        pressed = true
      elsif LCD.bottom_pressed?
        cy += 20 * dy
        pressed = true
      elsif LCD.center_pressed?
        let dx_old = dx
        let dy_old = dy
        dx = (dx * 0xB0000000) >> 32
        dy = (dy * 0xB0000000) >> 32
        cx += (dx_old - dx) * 64
        cy += (dy_old - dy) * 64
        pressed = true
      end
    end
  end
end

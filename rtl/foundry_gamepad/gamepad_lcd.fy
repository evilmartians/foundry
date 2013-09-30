class GamepadLCD
  LCD_CD    = 1
  LCD_nRST  = 2
  LCD_nCS   = 3
  LCD_nRD   = 4
  LCD_nWR   = 5
  LCD_D0    = 8

  def @framebuffer : Array(Unsigned(32), resizable: false)

  def initialize
    @framebuffer = Array(Unsigned(32), resizable: false).new(reserve: 1024)
  end

  def setup
    RCC.AHBENR = RCC.AHBENR.
        set_gpioben(true)

    # Configure pins
    GPIOB.as_output(LCD_CD).
          as_output(LCD_nRST).set(LCD_nRST, false).
          as_output(LCD_nCS).set(LCD_nCS, true).
          as_output(LCD_nRD).set(LCD_nRD, true).
          as_output(LCD_nWR).set(LCD_nWR, true)

    8.times((n) do
      GPIOB.as_output(LCD_D0 + n); nil
    end)

    # Reset display
    GPIOB.set(LCD_nRST, false)
    self.delay(5000) # 10ms
    GPIOB.set(LCD_nRST, true)
    self.delay(5000) # 10ms

    GPIOB.set(LCD_nCS, false)

    # Initialize display
    self.write(is_data: false, 0b10000001) # Set Vbias potentiometer
    self.write(is_data: false,         90) # = 96
    self.write(is_data: false, 0b11000110) # Set Mapping Control MX=1 MY=1
    self.write(is_data: false, 0b10101111) # Set Display Enable = 1

    # Clear display RAM
    self.refresh

    self
  end

  def write(is_data:, byte)
    GPIOB.ODR = GPIOB.ODR.set_range(byte, offset: LCD_D0, width: 8)

    GPIOB.set(LCD_CD, is_data).
          set(LCD_nWR, false)
    # 40ns

    GPIOB.set(LCD_nWR, true)
    # 40ns

    self
  end

  def clear
    1024u32.times((index) do
      @framebuffer[index] = 0
    end)
  end

  def set_pixel(x:, y:, on)
    let index = (y >> 3) * 128u32 + x
    let mask  = 1 << (y & 0x7)

    if on
      @framebuffer[index] |= mask
    else
      @framebuffer[index] &= ~mask
    end
  end

  def rectangle(x:, y:, w:, h:, on)
    w.times((dx) do
      h.times((dy) do
        self.set_pixel(x: x + dx, y: y + dy, on)
      end)
    end)
  end

  def refresh
    8u32.times((page) do
      self.write(is_data: false, 0b10110000 | page) # Set PA = page
      self.write(is_data: false, 0b00000000) # Set CA LSB = 0
      self.write(is_data: false, 0b00010000) # Set CA MSB = 0

      128u32.times((column) do
        let index = page * 128 + column
        self.write(is_data: true, @framebuffer[index])
      end)
    end)
  end

  def delay(n) : (\self, Unsigned(32)) -> Nil
    n.times((_) { RCC.AHBENR }) # silly
  end
end

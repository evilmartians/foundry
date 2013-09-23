class FoundryGamepad
  def delay(n) : (\self, Unsigned(32)) -> Nil
    n.times((_) { RCC.AHBENR }) # silly
  end

  KEY_LEFT  = 0
  KEY_RIGHT = 2
  KEY_UP    = 3
  KEY_DOWN  = 6
  KEY_B     = 8
  KEY_A     = 10

  BUZZ      = 7

  LCD_CD    = 1
  LCD_nRST  = 2
  LCD_nCS   = 3
  LCD_nRD   = 4
  LCD_nWR   = 5
  LCD_D0    = 8

  def setup
    RCC.AHBENR = RCC.AHBENR.
          set_gpioaen(true).
          set_gpioben(true)

    GPIOA.as_input(KEY_LEFT,  pull_down: true).
          as_input(KEY_RIGHT, pull_down: true).
          as_input(KEY_UP,    pull_down: true).
          as_input(KEY_DOWN,  pull_down: true).
          as_input(KEY_B,     pull_down: true).
          as_input(KEY_A,     pull_down: true).
          as_output(BUZZ)

    GPIOB.as_output(LCD_CD).
          as_output(LCD_nRST).set(LCD_nRST, false).
          as_output(LCD_nCS).set(LCD_nCS, true).
          as_output(LCD_nRD).set(LCD_nRD, true).
          as_output(LCD_nWR).set(LCD_nWR, true)

    8.times((n) do
      GPIOB.as_output(LCD_D0 + n)
    end)

    self.lcd_setup
  end

  def lcd_setup
    GPIOB.set(LCD_nRST, false)
    self.delay(5000) # 10ms
    GPIOB.set(LCD_nRST, true)
    self.delay(5000) # 10ms

    GPIOB.set(LCD_nCS, false)

    self.lcd_write(is_data: false, 0b10000001) # Set Vbias potentiometer
    self.lcd_write(is_data: false,         96) # = 96
    self.lcd_write(is_data: false, 0b11000110) # Set Mapping Control MX=1 MY=1
    self.lcd_write(is_data: false, 0b10101111) # Set Display Enable = 1

    # Clear display RAM
    (128u32 * 64).times((_) do
      self.lcd_write(0, is_data: true)
    end)

    self
  end

  def lcd_write(is_data:, byte)
    GPIOB.ODR = GPIOB.ODR.set_range(byte, offset: LCD_D0, width: 8)

    GPIOB.set(LCD_CD, is_data).
          set(LCD_nWR, false)
    # 40ns

    GPIOB.set(LCD_nWR, true)
    # 40ns

    self
  end

  def set_pixel(x:, y:, on)
    self.lcd_write(is_data: false, # Set PA
                   0b10110000 | (y >> 3) & 0xF)
    self.lcd_write(is_data: false, # Set CA LSB
                   0b00000000 | (x & 0xF))
    self.lcd_write(is_data: false, # Set CA MSB
                   0b00010000 | (x >> 4) & 0xF)
    self.lcd_write(is_data: true,
                   1 << (y & 0x7))
  end

  def up_pressed?
    GPIOA.get(KEY_UP)
  end

  def down_pressed?
    GPIOA.get(KEY_DOWN)
  end

  def left_pressed?
    GPIOA.get(KEY_LEFT)
  end

  def right_pressed?
    GPIOA.get(KEY_RIGHT)
  end

  def a_pressed?
    GPIOA.get(KEY_A)
  end

  def b_pressed?
    GPIOA.get(KEY_B)
  end
end

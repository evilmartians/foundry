class ColorLCDShield
  def delay(n)
    n.times((i) do
      RCC.APB2ENR # volatile do nothing.
    end)
  end

  def delay_60ns
    self.delay(4u32)
  end

  def spi_tx(is_data, mut byte)
    GPIOA.set(5_u32, false) # SCLK low
    GPIOB.set(7_u32, false) # CS low
    self.delay_60ns
    GPIOA.set(7_u32, is_data)
    GPIOA.set(5_u32, true)  # SCLK high
    self.delay_60ns

    let mut idx = 0u32
    while idx < 8u32
      GPIOA.set(5_u32, false) # SCLK low
      GPIOA.set(7_u32, byte & 0x80u8 == 0x80u8)
      self.delay_60ns
      GPIOA.set(5_u32, true)  # SCLK high
      self.delay_60ns

      byte <<= 1u8
      idx   += 1u32
    end

    GPIOB.set(7_u32, true)  # CS high
  end

  def setup
    RCC.APB2ENR = RCC.APB2ENR.
        set_iopaen(true).
        set_iopben(true).
        set_iopcen(true)

    # Set up LCD pins
    # cs    = D9
    GPIOB.as_output(7_u32, alternate: false, open_drain: false)
    GPIOB.set(7_u32, true)
    # reset = D10
    GPIOA.as_output(10_u32, alternate: false, open_drain: false)
    # mosi  = D11
    GPIOA.as_output(7_u32, alternate: false, open_drain: false)
    # sck   = D13
    GPIOA.as_output(5_u32, alternate: false, open_drain: false)

    # Backlight
    GPIOA.as_output(4_u32, alternate: false, open_drain: false)

    # Joystick
    GPIOC.as_input(0_u32, pull: true).
          as_input(1_u32, pull: true).
          as_input(2_u32, pull: true).
          as_input(3_u32, pull: true).
          as_input(4_u32, pull: true).
          set(0_u32, true).
          set(1_u32, true).
          set(2_u32, true).
          set(3_u32, true).
          set(4_u32, true)

    # Reset and initialize LCD
    GPIOA.set(10_u32, false)
    self.delay(500u32)
    GPIOA.set(10_u32, true)
    self.delay(500u32)

    self.spi_tx(false, 0x11u8) # sleep out

    self.spi_tx(false, 0x36u8) # memory mode
    self.spi_tx(true,  0xC0u8) # invert X/Y

    self.spi_tx(false, 0x3Au8) # color mode
    self.spi_tx(true,  0x05u8) # 16bpp

    self.spi_tx(false, 0x29u8) # display on
  end

  #
  # LCD API
  #

  def backlight_on
    GPIOA.get(4_u32)
  end

  def backlight_on=(value)
    GPIOA.set(4_u32, value)
  end

  def clip(x1:, y1:, x2:, y2:)
    self.spi_tx(false, 0x2Au8) # set columns
    self.spi_tx(true,  x1)
    self.spi_tx(true,  x2)

    self.spi_tx(false, 0x2Bu8) # set rows
    self.spi_tx(true,  y1)
    self.spi_tx(true,  y2)
  end

  def start
    self.spi_tx(false, 0x2Cu8)
  end

  def put_pixel(r, g, b)
    # R:G:B -> 5:6:5
    self.spi_tx(true, (r & 0xF8u8) | (g >> 5u8))
    self.spi_tx(true, (g & 0xE0u8) | (b >> 3u8))
  end

  def finish
    self.spi_tx(false, 0x00u8)
  end

  def clear(r, g, b)
    self.clip(x1: 2u8, x2: 130u8, y1: 2u8, y2: 130u8)
    self.start
    (130u32 * 130u32).times((_) do
      self.put_pixel(r, g, b)
    end)
    self.finish
  end

  #
  # JOYSTICK API
  #

  def left_pressed?
    not GPIOC.get(0_u32)
  end

  def top_pressed?
    not GPIOC.get(1_u32)
  end

  def right_pressed?
    not GPIOC.get(2_u32)
  end

  def bottom_pressed?
    not GPIOC.get(4_u32)
  end

  def center_pressed?
    not GPIOC.get(3_u32)
  end
end

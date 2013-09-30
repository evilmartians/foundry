class ColorLCDShield
  def delay(n) : (\a, Unsigned(32)) -> Nil
    n.times((i) do
      RCC.APB2ENR # volatile do nothing.
    end)

    nil
  end

  def delay_60ns
    self.delay(4)
  end

  def spi_tx(is_data, mut byte) : (\a, Boolean, Unsigned(8)) -> Nil
    GPIOA.set(5, false) # SCLK low
    GPIOB.set(7, false) # CS low
    self.delay_60ns
    GPIOA.set(7, is_data)
    GPIOA.set(5, true)  # SCLK high
    self.delay_60ns

    let mut idx = 0u32
    while idx < 8
      GPIOA.set(5, false) # SCLK low
      GPIOA.set(7, byte & 0x80 == 0x80)
      self.delay_60ns
      GPIOA.set(5, true)  # SCLK high
      self.delay_60ns

      byte <<= 1
      idx   += 1
    end

    GPIOB.set(7, true)  # CS high

    nil
  end

  def setup
    RCC.APB2ENR = RCC.APB2ENR.
        set_iopaen(true).
        set_iopben(true).
        set_iopcen(true)

    # Set up LCD pins
    # cs    = D9
    GPIOB.as_output(7, alternate: false, open_drain: false)
    GPIOB.set(7, true)
    # reset = D10
    GPIOA.as_output(10, alternate: false, open_drain: false)
    # mosi  = D11
    GPIOA.as_output(7, alternate: false, open_drain: false)
    # sck   = D13
    GPIOA.as_output(5, alternate: false, open_drain: false)

    # Backlight
    GPIOA.as_output(4, alternate: false, open_drain: false)

    # Joystick
    GPIOC.as_input(0, pull_up: true).
          as_input(1, pull_up: true).
          as_input(2, pull_up: true).
          as_input(3, pull_up: true).
          as_input(4, pull_up: true).
          set(0, true).
          set(1, true).
          set(2, true).
          set(3, true).
          set(4, true)

    # Reset and initialize LCD
    GPIOA.set(10, false)
    self.delay(500)
    GPIOA.set(10, true)
    self.delay(500)

    self.spi_tx(false, 0x11) # sleep out

    self.spi_tx(false, 0x36) # memory mode
    self.spi_tx(true,  0xC0) # invert X/Y

    self.spi_tx(false, 0x3A) # color mode
    self.spi_tx(true,  0x05) # 16bpp

    self.spi_tx(false, 0x29) # display on
  end

  #
  # LCD API
  #

  def backlight_on
    GPIOA.get(4)
  end

  def backlight_on=(value)
    GPIOA.set(4, value)
  end

  def clip(x1:, y1:, x2:, y2:)
    self.spi_tx(false, 0x2A) # set columns
    self.spi_tx(true,  x1)
    self.spi_tx(true,  x2)

    self.spi_tx(false, 0x2B) # set rows
    self.spi_tx(true,  y1)
    self.spi_tx(true,  y2)
  end

  def start
    self.spi_tx(false, 0x2C)
  end

  def put_pixel(r, g, b) : (\a, Unsigned(8), Unsigned(8), Unsigned(8)) -> Nil
    # R:G:B -> 5:6:5
    self.spi_tx(true, (r & 0xF8) | (g >> 5))
    self.spi_tx(true, (g & 0xE0) | (b >> 3))
  end

  def finish
    self.spi_tx(false, 0x00)
  end

  def clear(r, g, b)
    self.clip(x1: 2, x2: 130, y1: 2, y2: 130)
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
    !GPIOC.get(0)
  end

  def top_pressed?
    !GPIOC.get(1)
  end

  def right_pressed?
    !GPIOC.get(2)
  end

  def bottom_pressed?
    !GPIOC.get(4)
  end

  def center_pressed?
    !GPIOC.get(3)
  end
end

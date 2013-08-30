  #################################
  #   LANGUAGE STANDARD LIBRARY   #
  #################################

class Class
  def define_method(name, body)
    invokeprimitive cls_defm (self, name, body)
  end
end

class Value
  def self.new(*args, **kwargs)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args, **kwargs)
  end

  def initialize
  end

  def ==(other)
    invokeprimitive obj_equal(self, other)
  end
end

class Object
  def self.new(*args, **kwargs)
    let instance = invokeprimitive obj_alloc(self)
    instance.initialize(*args, **kwargs)
  end

  def initialize
  end
end

class Symbol
  def to_s
    invokeprimitive sym_to_str(self)
  end
end

class Unsigned
  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def !=(other)
    invokeprimitive int_ne(self, other)
  end

  def <(other)
    invokeprimitive int_ult(self, other)
  end

  def >(other)
    invokeprimitive int_ugt(self, other)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def -@
    0u32 - self
  end

  def ~@
    -self - 1u32
  end

  def |(other)
    invokeprimitive int_or(self, other)
  end

  def &(other)
    invokeprimitive int_and(self, other)
  end

  def <<(other)
    invokeprimitive int_shl(self, other)
  end

  def >>(other)
    invokeprimitive int_lshr(self, other)
  end

  def to_s
    invokeprimitive int_to_str(self)
  end

  def times(block)
    let mut i = 0u32
    while i < self
      block.call(i)
      i += 1u32
    end
  end
end

class Signed
  def ==(other)
    invokeprimitive int_eq(self, other)
  end

  def >(other)
    invokeprimitive int_sgt(self, other)
  end

  def <(other)
    invokeprimitive int_slt(self, other)
  end

  def -@
    invokeprimitive int_sub(0s32, self)
  end

  def +(other)
    invokeprimitive int_add(self, other)
  end

  def -(other)
    invokeprimitive int_sub(self, other)
  end

  def *(other)
    invokeprimitive int_mul(self, other)
  end

  def >>(other)
    invokeprimitive int_ashr(self, other)
  end
end

class Lambda
  def call(*args, **kwargs)
    invokeprimitive lam_call(self, args, kwargs)
  end
end

  #################################
  #   EMBEDDED STANDARD LIBRARY   #
  #################################

class Register(\width) < Value
  def @value : Unsigned(\width)

  def initialize(value)
    @value = value
  end

  def value
    @value
  end

  def |(values)
    let mut value = self

    invokeprimitive rec_enum(values, (field_name, field_value) do
      value = invokeprimitive obj_send(self, :"set_#{field_name}",
                                       value, field_value)
    end)

    value
  end

  def self.flag(name, kind, offset:)
    let mask = 1u32 << offset

    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        @value & mask != 0u32
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, field_value) do
        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end
  end

  def self.flags(name, kind, offset:, spacing:)
    if kind == :r or kind == :rw
      self.define_method(name, (self, n) do
        let mask = 1u32 << (offset + n * (1u32 + spacing))

        @value & mask != 0u32
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, n, field_value) do
        let mask = 1u32 << (offset + n * (1u32 + spacing))

        if field_value
          self { @value = @value | mask }
        else
          self { @value = @value & ~mask }
        end
      end)
    end
  end

  def self.field(name, kind, offset:, width:)
    let mask = (1u32 << width) - 1u32

    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        (@value >> offset) & mask
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, field_value) do
        self do
          @value = @value & ~(mask << offset) | ((field_value & mask) << offset)
        end
      end)
    end
  end

  def self.fields(name, kind, offset:, width:, spacing:)
    let mask = (1u32 << width) - 1u32

    if kind == :r or kind == :rw
      self.define_method(name, (self, n) do
        let field_offset = offset + n * (width + spacing)

        (@value >> field_offset) & mask
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"set_#{name}", (self, n, field_value) do
        let field_offset = offset + n * (width + spacing)

        self do
          @value = @value & ~(mask << field_offset) |
                            ((field_value & mask) << field_offset)
        end
      end)
    end
  end
end

class Unit < Value
  def @base : Unsigned(32)

  def initialize(base)
    @base = base
  end

  def self.register(name, cls, kind, offset, align:)
    if kind == :r or kind == :rw
      self.define_method(name, (self) do
        cls.new(invokeprimitive mem_loadv(align, @base + offset))
      end)
    end

    if kind == :w or kind == :rw
      self.define_method(:"#{name}=", (self, reg) do
        invokeprimitive mem_storev(align, @base + offset, reg.value)
      end)
    end
  end
end

  ################################
  #   STM32F1 STANDARD LIBRARY   #
  ################################

class RCC_CR < Register(32)
  self.flag(:hsion,    :rw, offset: 0u32)
  self.flag(:hsirdy,   :r,  offset: 1u32)
  self.field(:hsitrim, :rw, offset: 3u32, width: 5u32)
  self.field(:hsical,  :r,  offset: 8u32, width: 8u32)
  self.flag(:hseon,    :rw, offset: 16u32)
  self.flag(:hserdy,   :r,  offset: 17u32)
  self.flag(:hsebyp,   :rw, offset: 18u32)
  self.flag(:csson,    :rw, offset: 19u32)
  self.flag(:pllon,    :rw, offset: 24u32)
  self.flag(:pllrdy,   :r,  offset: 25u32)
end

class RCC_CFGR < Register(32)
  self.field(:sw,      :rw, offset: 0u32,  width: 2u32)
  self.field(:sws,     :r,  offset: 2u32,  width: 2u32)
  self.field(:hpre,    :rw, offset: 4u32,  width: 4u32)
  self.field(:ppre1,   :rw, offset: 8u32,  width: 3u32)
  self.field(:ppre2,   :rw, offset: 11u32, width: 3u32)
  self.field(:adcpre,  :rw, offset: 14u32, width: 2u32)
  self.flag(:pllsrc,   :rw, offset: 16u32)
  self.flag(:pllxtpre, :rw, offset: 17u32)
  self.field(:pllmul,  :rw, offset: 18u32, width: 4u32)
  self.flag(:usbpre,   :rw, offset: 22u32)
  self.field(:mco,     :rw, offset: 24u32, width: 3u32)
end

class RCC_APB2ENR < Register(32)
  self.flag(:afioen,   :rw, offset: 0u32)
  self.flag(:iopaen,   :rw, offset: 2u32)
  self.flag(:iopben,   :rw, offset: 3u32)
  self.flag(:iopcen,   :rw, offset: 4u32)
  self.flag(:iopden,   :rw, offset: 5u32)
  self.flag(:iopeen,   :rw, offset: 6u32)
  self.flag(:iopfen,   :rw, offset: 7u32)
  self.flag(:iopgen,   :rw, offset: 8u32)
  self.flag(:adc1en,   :rw, offset: 9u32)
  self.flag(:adc2en,   :rw, offset: 10u32)
  self.flag(:tim1en,   :rw, offset: 11u32)
  self.flag(:spi1en,   :rw, offset: 12u32)
  self.flag(:tim8en,   :rw, offset: 13u32)
  self.flag(:usart1en, :rw, offset: 14u32)
  self.flag(:adc3en,   :rw, offset: 15u32)
  self.flag(:tim9en,   :rw, offset: 19u32)
  self.flag(:tim10en,  :rw, offset: 20u32)
  self.flag(:tim11en,  :rw, offset: 21u32)
end

class RCCUnit < Unit
  self.register(:CR,      RCC_CR,      :rw, 0x00_u32, align: 1)
  self.register(:CFGR,    RCC_CFGR,    :rw, 0x04_u32, align: 1)
  self.register(:APB2ENR, RCC_APB2ENR, :rw, 0x18_u32, align: 1)
end

class GPIO_CR < Register(32)
  self.fields(:mode,   :rw, offset: 0u32, width: 2u32, spacing: 2u32)
  self.fields(:cnf,    :rw, offset: 2u32, width: 2u32, spacing: 2u32)
end

class GPIO_IDR < Register(32)
  self.flags(:idr,     :r,  offset: 0u32, spacing: 0u32)
end

class GPIO_ODR < Register(32)
  self.flags(:odr,     :rw, offset: 0u32, spacing: 0u32)
end

class GPIOUnit < Unit
  self.register(:CRL, GPIO_CR,  :rw, 0x00_u32, align: 4)
  self.register(:CRH, GPIO_CR,  :rw, 0x04_u32, align: 4)
  self.register(:IDR, GPIO_IDR, :r,  0x08_u32, align: 4)
  self.register(:ODR, GPIO_ODR, :rw, 0x0C_u32, align: 4)

  def pin_mode(n, mode, cnf)
    if n < 8u32
      self.CRL = self.CRL.set_mode(n, mode).
                          set_cnf(n, cnf)
    else
      self.CRH = self.CRH.set_mode(n - 8u32, mode).
                          set_cnf(n - 8u32, cnf)
    end

    self
  end

  def as_output(n, alternate:, open_drain:)
    let cnf  = (if alternate  then 2_u32 else 0_u32 end) |
               (if open_drain then 1_u32 else 0_u32 end)
    self.pin_mode(n, 1_u32, cnf)
  end

  def as_input(n, pull:)
    let cnf  = (if pull then 2_u32 else 1_u32 end)
    self.pin_mode(n, 0_u32, cnf)
  end

  def as_analog(n)
    self.pin_mode(n, 0_u32, 0_u32)
  end

  def set(n, is_high)
    self.ODR = self.ODR.set_odr(n, is_high)
    self
  end

  def get(n)
    self.IDR.idr(n)
  end
end

RCC   =  RCCUnit.new(0x4002_1000_u32)
GPIOA = GPIOUnit.new(0x4001_0800_u32)
GPIOB = GPIOUnit.new(0x4001_0C00_u32)
GPIOC = GPIOUnit.new(0x4001_1000_u32)
GPIOD = GPIOUnit.new(0x4001_1400_u32)
GPIOE = GPIOUnit.new(0x4001_1800_u32)
GPIOF = GPIOUnit.new(0x4001_1C00_u32)
GPIOG = GPIOUnit.new(0x4001_2000_u32)

  ##########################
  #   USER-AUTHORED CODE   #
  ##########################

class PCF8833 < Value
  def delay(n)
    n.times((i) do
      RCC.APB2ENR # volatile do nothing.
    end)
  end

  def spi_tx(is_data, mut byte)
    GPIOA.set(5_u32, false) # SCLK low
    GPIOB.set(7_u32, false) # CS low
    GPIOA.set(7_u32, is_data)
    GPIOA.set(5_u32, true)  # SCLK high

    let mut idx = 0u32
    while idx < 8u32
      GPIOA.set(5_u32, false) # SCLK low
      GPIOA.set(7_u32, byte & 0x80u8 == 0x80u8)
      GPIOA.set(5_u32, true)  # SCLK high

      byte <<= 1u8
      idx   += 1u32
    end

    GPIOB.set(7_u32, true)  # CS high
  end

  def reset
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

    GPIOA.set(10_u32, false)
    self.delay(50u32)
    GPIOA.set(10_u32, true)
    self.delay(50u32)

    self.spi_tx(false, 0x11u8) # sleep out

    self.spi_tx(false, 0x36u8) # memory mode
    self.spi_tx(true,  0xC0u8) # invert X/Y

    self.spi_tx(false, 0x3Au8) # color mode
    self.spi_tx(true,  0x05u8) # 16bpp

    self.spi_tx(false, 0x29u8) # display on
  end

  def clip(x1, y1, x2, y2)
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
    self.clip(2u8, 2u8, 130u8, 130u8)
    self.start
    (130u32 * 130u32).times((_) do
      self.put_pixel(r, g, b)
    end)
    self.finish
  end
end

def main
  # Switch to PLL
  RCC.CR = RCC.CR.set_hseon(true)
  while not RCC.CR.hserdy; end

  RCC.CFGR = RCC.CFGR.set_pllmul(0u32).set_pllsrc(true)
  RCC.CR   = RCC.CR.set_pllon(true)
  while not RCC.CR.pllrdy; end

  RCC.CFGR = RCC.CFGR.set_sw(2u32)
  while RCC.CFGR.sws != 2u32; end

  # Enable peripherals
  RCC.APB2ENR = RCC.APB2ENR.
      set_iopaen(true).
      set_iopben(true).
      set_iopcen(true)

  # Enable backlight
  GPIOA.as_output(4_u32, alternate: false, open_drain: false)
  GPIOA.set(4_u32, true)

  # Enable joystick
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

  # Initialize display
  let lcd = PCF8833.new
  lcd.reset
  lcd.clear(0xFFu8, 0xFFu8, 0xFFu8)

  let mut cx = 10u8
  let mut cy = 10u8

  while true
    lcd.clip(cx, cy, cx + 1u8, cy + 1u8)

    lcd.start
    let mut i = 0u32
    while i < 2u32 * 2u32
      if not GPIOC.get(0_u32)    # left
        lcd.put_pixel(0xFFu8, 0x00u8, 0x00u8)
      elsif not GPIOC.get(1_u32) # top
        lcd.put_pixel(0x00u8, 0xFFu8, 0x00u8)
      elsif not GPIOC.get(2_u32) # right
        lcd.put_pixel(0x00u8, 0x00u8, 0xFFu8)
      elsif not GPIOC.get(3_u32) # center
        lcd.put_pixel(0xFFu8, 0xFFu8, 0x00u8)
      elsif not GPIOC.get(4_u32) # bottom
        lcd.put_pixel(0xFFu8, 0x00u8, 0xFFu8)
      else
        lcd.put_pixel(0x00u8, 0x00u8, 0x00u8)
      end
      i += 1u32
    end
    lcd.finish

    if not GPIOC.get(0_u32)    # left
      cx -= 1u8
    elsif not GPIOC.get(1_u32) # top
      cy -= 1u8
    elsif not GPIOC.get(2_u32) # right
      cx += 1u8
    elsif not GPIOC.get(4_u32) # bottom
      cy += 1u8
    end
    lcd.delay(50000u32)
  end
end

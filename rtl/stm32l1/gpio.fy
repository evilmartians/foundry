class GPIOUnit < Unit
  class GPIO_MODER < Register(32)
    self.fields(:mode,   :rw, offset: 0,  width: 2, count: 16)
  end

  class GPIO_OTYPER < Register(32)
    self.flags(:otype,   :rw, offset: 0,  count: 16)
  end

  class GPIO_OSPEEDR < Register(32)
    self.fields(:ospeed, :rw, offset: 0,  width: 2, count: 16)
  end

  class GPIO_PUPDR < Register(32)
    self.fields(:pupd,   :rw, offset: 0,  width: 2, count: 16)
  end

  class GPIO_IDR < Register(32)
    self.flags(:data,    :r,  offset: 0,  count: 16)
  end

  class GPIO_ODR < Register(32)
    self.flags(:data,    :rw, offset: 0,  count: 16)

    def set_range(new_range, offset:, width:)
      let mask = ((1 << width) - 1) << offset
      self { @value = @value & ~mask | ((new_range << offset) & mask) }
    end
  end

  class GPIO_BSRR < Register(32)
    self.flags(:bs,      :w,  offset: 0,  count: 16)
    self.flags(:br,      :w,  offset: 16, count: 16)
  end

  class GPIO_LCKR < Register(32)
    self.flags(:lck,     :rw, offset: 0,  count: 16)
    self.flag(:lckk,     :rw, offset: 16)
  end

  class GPIO_AFR < Register(32)
    self.fields(:af,     :rw, offset: 0,  width: 4, count: 16)
  end

  self.register(:MODER,   :rw, offset: 0x00, align: 4, impl: GPIO_MODER)
  self.register(:OTYPER,  :rw, offset: 0x04, align: 4, impl: GPIO_OTYPER)
  self.register(:OSPEEDR, :rw, offset: 0x08, align: 4, impl: GPIO_OSPEEDR)
  self.register(:PUPDR,   :rw, offset: 0x0C, align: 4, impl: GPIO_PUPDR)
  self.register(:IDR,     :rw, offset: 0x10, align: 4, impl: GPIO_IDR)
  self.register(:ODR,     :rw, offset: 0x14, align: 4, impl: GPIO_ODR)
  self.register(:BSRR,    :w,  offset: 0x18, align: 4, impl: GPIO_BSRR)
  self.register(:LCKR,    :rw, offset: 0x1C, align: 4, impl: GPIO_LCKR)
  self.register(:AFLR,    :rw, offset: 0x20, align: 4, impl: GPIO_AFR)
  self.register(:AFHR,    :rw, offset: 0x24, align: 4, impl: GPIO_AFR)

  def BSRR
    GPIO_BSRR.new(0)
  end

  def pull(n, pull_up:, pull_down:)
    let pupd = (if    pull_up   then 0b01
                elsif pull_down then 0b10
                else  0b00
                end)
    self.PUPDR = self.PUPDR.set_pupd(n, pupd)

    self
  end

  def as_output(n, alternate: false, open_drain: false,
                   pull_up: false, pull_down: false)
    let mode = (if alternate then 0b10 else 0b01 end)
    self.MODER  = self.MODER.set_mode(n, mode)
    self.OTYPER = self.OTYPER.set_otype(n, open_drain)

    self.pull(n, pull_up:, pull_down:)
  end

  def as_input(n, pull_up: false, pull_down: false)
    self.MODER = self.MODER.set_mode(n, 0b00)

    self.pull(n, pull_up:, pull_down:)
  end

  def as_analog(n)
    self.MODER = self.MODER.set_mode(n, 0b11)

    self
  end

  def set(n, is_high)
    #if is_high
    #  self.BSRR = self.BSRR.set_bs(n, true)
    #else
    #  self.BSRR = self.BSRR.set_br(n, true)
    #end
    self.ODR = self.ODR.set_data(n, is_high)

    self
  end

  def get(n)
    self.IDR.data(n)
  end
end

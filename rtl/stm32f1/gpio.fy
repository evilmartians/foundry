class GPIOUnit < Unit
  class GPIO_CR < Register(32)
    self.fields(:mode, :rw, offset: 0,  count: 16, width: 2, spacing: 2)
    self.fields(:cnf,  :rw, offset: 2,  count: 16, width: 2, spacing: 2)
  end

  class GPIO_IDR < Register(32)
    self.flags(:data,  :r,  offset: 0,  count: 16)
  end

  class GPIO_ODR < Register(32)
    self.flags(:data,  :rw, offset: 0,  count: 16)
  end

  class GPIO_BSRR < Register(32)
    self.flags(:bs,    :w,  offset: 0,  count: 16)
    self.flags(:br,    :w,  offset: 16, count: 16)
  end

  class GPIO_BRR < Register(32)
    self.flags(:br,    :w,  offset: 0,  count: 16)
  end

  class GPIO_LCKR < Register(32)
    self.flags(:lck,   :rw, offset: 0,  count: 16)
    self.flag(:lckk,   :rw, offset: 16)
  end

  self.register(:CRL,  :rw, offset: 0x00, align: 4, impl: GPIO_CR)
  self.register(:CRH,  :rw, offset: 0x04, align: 4, impl: GPIO_CR)
  self.register(:IDR,  :r,  offset: 0x08, align: 4, impl: GPIO_IDR)
  self.register(:ODR,  :rw, offset: 0x0C, align: 4, impl: GPIO_ODR)
  self.register(:BSRR, :rw, offset: 0x10, align: 4, impl: GPIO_BSRR)
  self.register(:BRR,  :rw, offset: 0x14, align: 4, impl: GPIO_BRR)
  self.register(:LCKR, :rw, offset: 0x18, align: 4, impl: GPIO_LCKR)

  def pin_mode(n, mode, cnf)
    if n < 8
      self.CRL = self.CRL.set_mode(n, mode).
                          set_cnf(n, cnf)
    else
      self.CRH = self.CRH.set_mode(n - 8, mode).
                          set_cnf(n - 8, cnf)
    end

    self
  end

  def as_output(n, alternate: false, open_drain: false)
    let cnf  = (if alternate  then 2 else 0 end) |
               (if open_drain then 1 else 0 end)

    self.pin_mode(n, 1, cnf)
  end

  def as_input(n, pull_up: false)
    let cnf  = (if pull_up then 2 else 1 end)

    self.pin_mode(n, 0, cnf)
  end

  def as_analog(n)
    self.pin_mode(n, 0, 0)
  end

  def set(n, is_high)
    if is_high
      self.BSRR = self.BSRR.set_bs(n, true)
    else
      self.BRR  = self.BRR.set_br(n, true)
    end

    self
  end

  def get(n)
    self.IDR.data(n)
  end
end

class GPIOUnit < Unit
  class GPIO_CR < Register(32)
    self.fields(:mode, :rw, offset: 0, count: 16, width: 2, spacing: 2)
    self.fields(:cnf,  :rw, offset: 2, count: 16, width: 2, spacing: 2)
  end

  class GPIO_IDR < Register(32)
    self.flags(:idr,   :r,  offset: 0, count: 32)
  end

  class GPIO_ODR < Register(32)
    self.flags(:odr,   :rw, offset: 0, count: 32)
  end

  self.register(:CRL,  :rw, offset: 0x00, align: 4, impl: GPIO_CR)
  self.register(:CRH,  :rw, offset: 0x04, align: 4, impl: GPIO_CR)
  self.register(:IDR,  :r,  offset: 0x08, align: 4, impl: GPIO_IDR)
  self.register(:ODR,  :rw, offset: 0x0C, align: 4, impl: GPIO_ODR)

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

  def as_output(n, alternate:, open_drain:)
    let cnf  = (if alternate  then 2 else 0 end) |
               (if open_drain then 1 else 0 end)
    self.pin_mode(n, 1, cnf)
  end

  def as_input(n, pull:)
    let cnf  = (if pull then 2 else 1 end)
    self.pin_mode(n, 0, cnf)
  end

  def as_analog(n)
    self.pin_mode(n, 0, 0)
  end

  def set(n, is_high)
    self.ODR = self.ODR.set_odr(n, is_high)
    self
  end

  def get(n)
    self.IDR.idr(n)
  end
end

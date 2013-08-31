class GPIOUnit < Unit
  class GPIO_CR < Register(32)
    self.fields(:mode, :rw, offset: 0u32, width: 2u32, spacing: 2u32)
    self.fields(:cnf,  :rw, offset: 2u32, width: 2u32, spacing: 2u32)
  end

  class GPIO_IDR < Register(32)
    self.flags(:idr,   :r,  offset: 0u32, spacing: 0u32)
  end

  class GPIO_ODR < Register(32)
    self.flags(:odr,   :rw, offset: 0u32, spacing: 0u32)
  end

  self.register(:CRL,  :rw, offset: 0x00_u32, align: 4, impl: GPIO_CR)
  self.register(:CRH,  :rw, offset: 0x04_u32, align: 4, impl: GPIO_CR)
  self.register(:IDR,  :r,  offset: 0x08_u32, align: 4, impl: GPIO_IDR)
  self.register(:ODR,  :rw, offset: 0x0C_u32, align: 4, impl: GPIO_ODR)

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

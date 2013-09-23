class PWRUnit < Unit
  class PWR_CR < Register(32)
    self.flag(:lpds,  :rw,    offset: 0)
    self.flag(:pdds,  :rw,    offset: 1)
    self.flag(:cwuf,  :rc_w1, offset: 2)
    self.flag(:csbf,  :rc_w1, offset: 3)
    self.flag(:pvde,  :rw,    offset: 4)
    self.field(:pls,  :rw,    offset: 5, width: 3)
    self.flag(:dbp,   :rw,    offset: 8)
  end

  class PWR_CSR < Register(32)
    self.flag(:wuf,   :r,     offset: 0)
    self.flag(:sbf,   :r,     offset: 1)
    self.flag(:pvdo,  :r,     offset: 2)
    self.flag(:ewup,  :rw,    offset: 8)
  end

  self.register(:CR,  :rw, offset: 0x00, align: 2, impl: PWR_CR)
  self.register(:CSR, :rw, offset: 0x04, align: 2, impl: PWR_CSR)
end

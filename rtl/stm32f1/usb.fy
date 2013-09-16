class USBUnit < Unit
  class USB_CNTR < Register(16)
    self.flag(:fres,    :rw, offset: 0)
    self.flag(:pdwn,    :rw, offset: 1)
    self.flag(:lp_mode, :rw, offset: 2)
    self.flag(:fsusp,   :rw, offset: 3)
    self.flag(:resume,  :rw, offset: 4)
    self.flag(:esofm,   :rw, offset: 8)
    self.flag(:sofm,    :rw, offset: 9)
    self.flag(:resetm,  :rw, offset: 10)
    self.flag(:suspm,   :rw, offset: 11)
    self.flag(:wkupm,   :rw, offset: 12)
    self.flag(:errm,    :rw, offset: 13)
    self.flag(:pmaovrm, :rw, offset: 14)
    self.flag(:ctrm,    :rw, offset: 15)
  end

  class USB_ISTR < Register(16)
    self.field(:ep_id,  :r,     offset: 0, width: 4)
    self.flag(:dir,     :r,     offset: 4)
    self.flag(:esof,    :rc_w0, offset: 8)
    self.flag(:sof,     :rc_w0, offset: 9)
    self.flag(:reset,   :rc_w0, offset: 10)
    self.flag(:susp,    :rc_w0, offset: 11)
    self.flag(:wkup,    :rc_w0, offset: 12)
    self.flag(:err,     :rc_w0, offset: 13)
    self.flag(:pmaovr,  :rc_w0, offset: 14)
    self.flag(:ctr,     :r,     offset: 15)
  end

  class USB_FNR < Register(16)
    self.field(:fn,     :r,     offset: 0,  width: 11)
    self.field(:lsof,   :r,     offset: 11, width: 2)
    self.flag(:lck,     :r,     offset: 13)
    self.flag(:rxdm,    :r,     offset: 14)
    self.flag(:rxdp,    :r,     offset: 15)
  end

  class USB_DADDR < Register(16)
    self.field(:addr,   :rw,    offset: 0,  width: 7)
    self.flag(:ef,      :rw,    offset: 8)
  end

  class USB_EPnR < Register(16)
    self.field(:ea,       :rw,    offset: 0, width: 4)
    self.field(:stat_tx,  :t,     offset: 4, width: 2)
    self.flag(:dtog_tx,   :t,     offset: 6)
    self.flag(:ctr_tx,    :rc_w0, offset: 7)
    self.flag(:ep_kind,   :rw,    offset: 8)
    self.field(:ep_type,  :rw,    offset: 9,  width: 2)
    self.flag(:setup,     :r,     offset: 11)
    self.field(:stat_rx,  :t,     offset: 12, width: 2)
    self.flag(:dtog_rx,   :t,     offset: 14)
    self.flag(:ctr_rx,    :rc_w0, offset: 15)
  end

  self.registers(:EPnR, :rwc, offset: 0x00, count: 8, align: 4, impl: USB_EPnR,
                              invariant_set:   0b1000_0000_1000_0000,
                              invariant_clear: 0b0111_0000_0111_0000)
  self.register(:CNTR,  :rw,  offset: 0x40, align: 4, impl: USB_CNTR)
  self.register(:ISTR,  :rwc, offset: 0x44, align: 4, impl: USB_ISTR,
                              invariant_set:   0b0111_1111_0000_0000)
  self.register(:FNR,   :r,   offset: 0x48, align: 4, impl: USB_FNR)
  self.register(:DADDR, :r,   offset: 0x4C, align: 4, impl: USB_DADDR)
end

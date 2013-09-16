class AFIOUnit < Unit
  class AFIO_EVCR < Register(32)
    self.field(:pin,  :rw, offset: 0, width: 4)
    self.field(:port, :rw, offset: 4, width: 4)
    self.flag(:evoe,  :rw, offset: 7)
  end

  class AFIO_MAPR < Register(32)
    self.flag(:spi1_remap,        :rw, offset: 0)
    self.flag(:i2c1_remap,        :rw, offset: 1)
    self.flag(:usart1_remap,      :rw, offset: 2)
    self.flag(:usart2_remap,      :rw, offset: 3)
    self.field(:usart3_remap,     :rw, offset: 4,  width: 2)
    self.field(:tim1_remap,       :rw, offset: 6,  width: 2)
    self.field(:tim2_remap,       :rw, offset: 8,  width: 2)
    self.field(:tim3_remap,       :rw, offset: 10, width: 2)
    self.flag(:tim4_remap,        :rw, offset: 12)
    self.field(:can_remap,        :rw, offset: 13, width: 2)
    self.flag(:pd01_remap,        :rw, offset: 15)
    self.flag(:tim5ch4_iremap,    :rw, offset: 16)
    self.flag(:adc1etrginj_remap, :rw, offset: 17)
    self.flag(:adc1etrgreg_remap, :rw, offset: 18)
    self.flag(:adc2etrginj_remap, :rw, offset: 19)
    self.flag(:adc2etrgreg_remap, :rw, offset: 20)
    self.field(:swj_cfg,          :w,  offset: 24, width: 3)
  end

  class AFIO_EXTICR < Register(32)
    self.fields(:exti, :rw, offset: 0, count: 4, width: 4)
  end

  class AFIO_MAPR2 < Register(32)
    self.flag(:tim9_remap,  :rw, offset: 5)
    self.flag(:tim10_remap, :rw, offset: 6)
    self.flag(:tim11_remap, :rw, offset: 7)
    self.flag(:tim13_remap, :rw, offset: 8)
    self.flag(:tim14_remap, :rw, offset: 9)
    self.flag(:fsmc_nadv,   :rw, offset: 10)
  end

  self.register(:EVCR,    :rw, offset: 0x00, align: 4, impl: AFIO_EVCR)
  self.register(:MAPR,    :rw, offset: 0x04, align: 4, impl: AFIO_MAPR)
  self.registers(:EXTICR, :rw, offset: 0x08, count: 4, align: 4, impl: AFIO_EXTICR)
  self.register(:MAPR2,   :rw, offset: 0x1C, align: 4, impl: AFIO_MAPR2)
end

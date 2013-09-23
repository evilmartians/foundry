class RCCUnit < Unit
  class RCC_CR < Register(32)
    self.flag(:hsion,   :rw, offset: 0)
    self.flag(:hsirdy,  :r,  offset: 1)
    self.flag(:msion,   :rw, offset: 8)
    self.flag(:msirdy,  :r,  offset: 9)
    self.flag(:hseon,   :rw, offset: 16)
    self.flag(:hserdy,  :r,  offset: 17)
    self.flag(:hsebyp,  :rw, offset: 18)
    self.flag(:pllon,   :rw, offset: 24)
    self.flag(:pllrdy,  :r,  offset: 25)
    self.flag(:csson,   :rw, offset: 28)
    self.field(:rtcpre, :rw, offset: 29, width: 2)
  end

  class RCC_ICSCR < Register(32)
    self.field(:hsical,   :r,  offset: 0,  width: 8)
    self.field(:hsitrim,  :rw, offset: 8,  width: 5)
    self.field(:msirange, :rw, offset: 13, width: 3)
    self.field(:msical,   :r,  offset: 16, width: 8)
    self.field(:msitrim,  :rw, offset: 24, width: 8)
  end

  class RCC_CFGR < Register(32)
    self.field(:sw,      :rw, offset: 0,  width: 2)
    self.field(:sws,     :r,  offset: 2,  width: 2)
    self.field(:hpre,    :rw, offset: 4,  width: 4)
    self.field(:ppre1,   :rw, offset: 8,  width: 3)
    self.field(:ppre2,   :rw, offset: 11, width: 3)
    self.field(:adcpre,  :rw, offset: 14, width: 2)
    self.flag(:pllsrc,   :rw, offset: 16)
    self.field(:pllmul,  :rw, offset: 18, width: 4)
    self.field(:plldiv,  :rw, offset: 22, width: 2)
    self.field(:mcosel,  :rw, offset: 24, width: 3)
    self.field(:mcopre,  :rw, offset: 28, width: 3)
  end

  class RCC_CIR < Register(32)
    self.flag(:lsirdyf,  :r,  offset: 0)
    self.flag(:lserdyf,  :r,  offset: 1)
    self.flag(:hsirdyf,  :r,  offset: 2)
    self.flag(:hserdyf,  :r,  offset: 3)
    self.flag(:pllrdyf,  :r,  offset: 4)
    self.flag(:msirdyf,  :r,  offset: 5)
    self.flag(:lsecssf,  :r,  offset: 6)
    self.flag(:cssf,     :r,  offset: 7)
    self.flag(:lsirdyie, :rw, offset: 8)
    self.flag(:lserdyie, :rw, offset: 9)
    self.flag(:hsirdyie, :rw, offset: 10)
    self.flag(:hserdyie, :rw, offset: 11)
    self.flag(:pllrdyie, :rw, offset: 12)
    self.flag(:msirdyie, :rw, offset: 13)
    self.flag(:lsecssie, :rw, offset: 14)
    self.flag(:lsirdyc,  :w,  offset: 16)
    self.flag(:lserdyc,  :w,  offset: 17)
    self.flag(:hsirdyc,  :w,  offset: 18)
    self.flag(:hserdyc,  :w,  offset: 19)
    self.flag(:pllrdyc,  :w,  offset: 20)
    self.flag(:msirdyc,  :w,  offset: 21)
    self.flag(:lsecssc,  :w,  offset: 22)
    self.flag(:cssc,     :w,  offset: 23)
  end

  class RCC_AHBRSTR < Register(32)
    self.flag(:gpioarst, :rw, offset: 0)
    self.flag(:gpiobrst, :rw, offset: 1)
    self.flag(:gpiocrst, :rw, offset: 2)
    self.flag(:gpiodrst, :rw, offset: 3)
    self.flag(:gpioerst, :rw, offset: 4)
    self.flag(:gpiofrst, :rw, offset: 5)
    self.flag(:gpiogrst, :rw, offset: 6)
    self.flag(:gpiohrst, :rw, offset: 7)
    self.flag(:crcrst,   :rw, offset: 12)
    self.flag(:flitfrst, :rw, offset: 15)
    self.flag(:dma1rst,  :rw, offset: 24)
    self.flag(:dma2rst,  :rw, offset: 25)
    self.flag(:aesrst,   :rw, offset: 27)
    self.flag(:fsmcrst,  :rw, offset: 30)
  end

  class RCC_APB2RSTR < Register(32)
    self.flag(:syscfgrst, :rw, offset: 0)
    self.flag(:tim9rst,   :rw, offset: 2)
    self.flag(:tim10rst,  :rw, offset: 3)
    self.flag(:tim11rst,  :rw, offset: 4)
    self.flag(:adc1rst,   :rw, offset: 9)
    self.flag(:sdiorst,   :rw, offset: 11)
    self.flag(:spi1rst,   :rw, offset: 12)
    self.flag(:usart1rst, :rw, offset: 14)
  end

  class RCC_APB1RSTR < Register(32)
    self.flag(:tim2rst,   :rw, offset: 0)
    self.flag(:tim3rst,   :rw, offset: 1)
    self.flag(:tim4rst,   :rw, offset: 2)
    self.flag(:tim5rst,   :rw, offset: 3)
    self.flag(:tim6rst,   :rw, offset: 4)
    self.flag(:tim7rst,   :rw, offset: 5)
    self.flag(:lcdrst,    :rw, offset: 9)
    self.flag(:wwdgrst,   :rw, offset: 11)
    self.flag(:spi2rst,   :rw, offset: 14)
    self.flag(:spi3rst,   :rw, offset: 15)
    self.flag(:usart2rst, :rw, offset: 17)
    self.flag(:usart3rst, :rw, offset: 18)
    self.flag(:usart4rst, :rw, offset: 19)
    self.flag(:usart5rst, :rw, offset: 20)
    self.flag(:i2c1rst,   :rw, offset: 21)
    self.flag(:i2c2rst,   :rw, offset: 22)
    self.flag(:usbrst,    :rw, offset: 23)
    self.flag(:pwrrst,    :rw, offset: 28)
    self.flag(:dacrst,    :rw, offset: 29)
    self.flag(:comprst,   :rw, offset: 31)
  end

  class RCC_AHBENR < Register(32)
    self.flag(:gpioaen,  :rw, offset: 0)
    self.flag(:gpioben,  :rw, offset: 1)
    self.flag(:gpiocen,  :rw, offset: 2)
    self.flag(:gpioden,  :rw, offset: 3)
    self.flag(:gpioeen,  :rw, offset: 4)
    self.flag(:gpiofen,  :rw, offset: 5)
    self.flag(:gpiogen,  :rw, offset: 6)
    self.flag(:gpiohen,  :rw, offset: 7)
    self.flag(:crcen,    :rw, offset: 12)
    self.flag(:flitfen,  :rw, offset: 15)
    self.flag(:dma1en,   :rw, offset: 24)
    self.flag(:dma2en,   :rw, offset: 25)
    self.flag(:aesen,    :rw, offset: 27)
    self.flag(:fsmcen,   :rw, offset: 30)
  end

  class RCC_APB2ENR < Register(32)
    self.flag(:syscfgen, :rw, offset: 0)
    self.flag(:tim9en,   :rw, offset: 2)
    self.flag(:tim10en,  :rw, offset: 3)
    self.flag(:tim11en,  :rw, offset: 4)
    self.flag(:adc1en,   :rw, offset: 9)
    self.flag(:sdioen,   :rw, offset: 11)
    self.flag(:spi1en,   :rw, offset: 12)
    self.flag(:usart1en, :rw, offset: 14)
  end

  class RCC_APB1ENR < Register(32)
    self.flag(:tim2en,   :rw, offset: 0)
    self.flag(:tim3en,   :rw, offset: 1)
    self.flag(:tim4en,   :rw, offset: 2)
    self.flag(:tim5en,   :rw, offset: 3)
    self.flag(:tim6en,   :rw, offset: 4)
    self.flag(:tim7en,   :rw, offset: 5)
    self.flag(:lcden,    :rw, offset: 9)
    self.flag(:wwdgen,   :rw, offset: 11)
    self.flag(:spi2en,   :rw, offset: 14)
    self.flag(:spi3en,   :rw, offset: 15)
    self.flag(:usart2en, :rw, offset: 17)
    self.flag(:usart3en, :rw, offset: 18)
    self.flag(:usart4en, :rw, offset: 19)
    self.flag(:usart5en, :rw, offset: 20)
    self.flag(:i2c1en,   :rw, offset: 21)
    self.flag(:i2c2en,   :rw, offset: 22)
    self.flag(:usben,    :rw, offset: 23)
    self.flag(:pwren,    :rw, offset: 28)
    self.flag(:dacen,    :rw, offset: 29)
    self.flag(:compen,   :rw, offset: 31)
  end

  class RCC_CSR < Register(32)
    self.flag(:lsion,     :rw, offset: 0)
    self.flag(:lsirdy,    :r,  offset: 1)
    self.flag(:lseon,     :rw, offset: 8)
    self.flag(:lserdy,    :r,  offset: 9)
    self.flag(:lsebyp,    :rw, offset: 10)
    self.flag(:lsecsson,  :rw, offset: 11)
    self.flag(:lsecssd,   :r,  offset: 12)
    self.field(:rtcsel,   :rw, offset: 16, width: 2)
    self.flag(:rtcen,     :rw, offset: 22)
    self.flag(:rtcrst,    :rw, offset: 23)
    self.flag(:rmvf,      :rw, offset: 24)
    self.flag(:oblrstf,   :rw, offset: 25)
    self.flag(:pinrstf,   :rw, offset: 26)
    self.flag(:porrstf,   :rw, offset: 27)
    self.flag(:sftrstf,   :rw, offset: 28)
    self.flag(:iwdgrstf,  :rw, offset: 29)
    self.flag(:wwdgrstf,  :rw, offset: 30)
    self.flag(:lpwrrstf,  :rw, offset: 31)
  end

  self.register(:CR,        :rw, offset: 0x00, align: 1, impl: RCC_CR)
  self.register(:ICSCR,     :rw, offset: 0x04, align: 1, impl: RCC_ICSCR)
  self.register(:CFGR,      :rw, offset: 0x08, align: 1, impl: RCC_CFGR)
  self.register(:CIR,       :rw, offset: 0x0C, align: 1, impl: RCC_CIR)
  self.register(:AHBRSTR,   :rw, offset: 0x10, align: 1, impl: RCC_AHBRSTR)
  self.register(:APB2RSTR,  :rw, offset: 0x14, align: 1, impl: RCC_APB2RSTR)
  self.register(:APB1RSTR,  :rw, offset: 0x18, align: 1, impl: RCC_APB1RSTR)
  self.register(:AHBENR,    :rw, offset: 0x1C, align: 1, impl: RCC_AHBENR)
  self.register(:APB2ENR,   :rw, offset: 0x20, align: 1, impl: RCC_APB2ENR)
  self.register(:APB1ENR,   :rw, offset: 0x24, align: 1, impl: RCC_APB1ENR)
  self.register(:AHBLPENR,  :rw, offset: 0x28, align: 1, impl: RCC_AHBENR)
  self.register(:APB2LPENR, :rw, offset: 0x2C, align: 1, impl: RCC_APB2ENR)
  self.register(:APB1LPENR, :rw, offset: 0x30, align: 1, impl: RCC_APB1ENR)
  self.register(:CSR,       :rw, offset: 0x34, align: 1, impl: RCC_CSR)

  def enable_hse
    RCC.CR = RCC.CR.set_hseon(true)
    while !RCC.CR.hserdy; end

    self
  end

  def enable_hsi
    RCC.CR = RCC.CR.set_hsion(true)
    while !RCC.CR.hsirdy; end

    self
  end

  def enable_pll(mul:, div:, use_hse:)
    RCC.CFGR = RCC.CFGR.set_pllmul(mul).
                        set_plldiv(div).
                        set_pllsrc(use_hse)

    RCC.CR   = RCC.CR.set_pllon(true)
    while !RCC.CR.pllrdy; end

    self
  end

  def switch(source)
    RCC.CFGR = RCC.CFGR.set_sw(source)
    while RCC.CFGR.sws != source; end

    self
  end
end

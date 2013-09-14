class MapleLeaf
  def setup
    RCC.CR = RCC.CR.set_hseon(true)
    while !RCC.CR.hserdy; end

    RCC.CFGR = RCC.CFGR.set_pllmul(0b1111).set_pllsrc(true)
    RCC.CR   = RCC.CR.set_pllon(true)
    while !RCC.CR.pllrdy; end

    RCC.CFGR = RCC.CFGR.set_sw(0b10)
    while RCC.CFGR.sws != 0b10; end
  end
end

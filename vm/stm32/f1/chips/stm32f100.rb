module STM32
  module F100RB
    RCC = F1::RCCUnit.reify(base: 0x4001_0000).new
  end
end

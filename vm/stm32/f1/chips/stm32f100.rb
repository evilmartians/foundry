module STM32::F100RB
  include STM32::F1

  RCC = RCCUnit.reify(base: 0x4001_0000)
end
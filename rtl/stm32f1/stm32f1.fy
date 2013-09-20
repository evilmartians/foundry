RCC   =  RCCUnit.new(0x4002_1000_u32)
AFIO  = AFIOUnit.new(0x4001_0000_u32)
GPIOA = GPIOUnit.new(0x4001_0800_u32)
GPIOB = GPIOUnit.new(0x4001_0C00_u32)
GPIOC = GPIOUnit.new(0x4001_1000_u32)
GPIOD = GPIOUnit.new(0x4001_1400_u32)
GPIOE = GPIOUnit.new(0x4001_1800_u32)
GPIOF = GPIOUnit.new(0x4001_1C00_u32)
GPIOG = GPIOUnit.new(0x4001_2000_u32)

def use_EXTI
  EXTI = EXTIUnit.new(0x4001_0400_u32)

  self.export(:_exti0, () { EXTI.handle_interrupt(0) })
  self.export(:_exti1, () { EXTI.handle_interrupt(1) })
  self.export(:_exti2, () { EXTI.handle_interrupt(2) })
  self.export(:_exti3, () { EXTI.handle_interrupt(3) })
  self.export(:_exti4, () { EXTI.handle_interrupt(4) })
end

def use_USB
  USB = USBUnit.new(0x4000_5c00_u32, buffer_base: 0x4000_6000_u32)

  let handle_usb = () { USB.handle_interrupt }
  #self.export(:_usb_hp_can_tx,  handle_usb)
  #self.export(:_usb_hp_can_rx0, handle_usb)
end

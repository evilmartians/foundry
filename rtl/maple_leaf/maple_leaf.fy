class MapleLeaf
  XTAL_FREQ = 8_000_000

  def setup_pll(frequency)
    RCC.enable_hse
    RCC.enable_pll(mul: frequency / XTAL_FREQ - 1, div: false,
                   use_hse: true)

    # Switch to PLL as system clock.
    RCC.switch(0b10)

    self
  end

  def setup_usb
    self.setup_pll(48_000_000)

    # Route USBDP/DM pins to the USB peripheral.
    RCC.APB2ENR = RCC.APB2ENR.set_iopaen(true)
    GPIOA.as_output(11, alternate: true, open_drain: false).
          as_output(12, alternate: true, open_drain: false)

    # Enable USB peripheral.
    RCC.APB1ENR = RCC.APB1ENR.set_usben(true)
    USB.power_up

    # Enable device detection pull-up resistor.
    RCC.APB2ENR = RCC.APB2ENR.set_iopcen(true)
    GPIOC.as_output(12, alternate: false, open_drain: false).
          set(12, false)

    # Set up a control endpoint.
    USB.setup_control(tx_length: 8, rx_length: 8)

    self
  end
end

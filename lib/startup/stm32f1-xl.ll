declare extern_weak void @_wwdg()
declare extern_weak void @_pvd()
declare extern_weak void @_tamper()
declare extern_weak void @_rtc()
declare extern_weak void @_flash()
declare extern_weak void @_rcc()
declare extern_weak void @_exti0()
declare extern_weak void @_exti1()
declare extern_weak void @_exti2()
declare extern_weak void @_exti3()
declare extern_weak void @_exti4()
declare extern_weak void @_dma1_channel1()
declare extern_weak void @_dma1_channel2()
declare extern_weak void @_dma1_channel3()
declare extern_weak void @_dma1_channel4()
declare extern_weak void @_dma1_channel5()
declare extern_weak void @_dma1_channel6()
declare extern_weak void @_dma1_channel7()
declare extern_weak void @_adc1_2()
declare extern_weak void @_usb_hp_can_tx()
declare extern_weak void @_usb_lp_can_rx0()
declare extern_weak void @_can_rx1()
declare extern_weak void @_can_sce()
declare extern_weak void @_exti9_5()
declare extern_weak void @_tim1_brk_tim9()
declare extern_weak void @_tim1_up_tim10()
declare extern_weak void @_tim1_trg_com_tim11()
declare extern_weak void @_tim1_cc()
declare extern_weak void @_tim2()
declare extern_weak void @_tim3()
declare extern_weak void @_tim4()
declare extern_weak void @_i2c1_ev()
declare extern_weak void @_i2c1_er()
declare extern_weak void @_i2c2_ev()
declare extern_weak void @_i2c2_er()
declare extern_weak void @_spi1()
declare extern_weak void @_spi2()
declare extern_weak void @_usart1()
declare extern_weak void @_usart2()
declare extern_weak void @_usart3()
declare extern_weak void @_exti15_10()
declare extern_weak void @_rtcalarm()
declare extern_weak void @_usbwakeup()
declare extern_weak void @_tim8_brk_tim12()
declare extern_weak void @_tim8_up_tim13()
declare extern_weak void @_tim8_trg_com_tim14()
declare extern_weak void @_tim8_cc()
declare extern_weak void @_adc3()
declare extern_weak void @_fsmc()
declare extern_weak void @_sdio()
declare extern_weak void @_tim5()
declare extern_weak void @_spi3()
declare extern_weak void @_uart4()
declare extern_weak void @_uart5()
declare extern_weak void @_tim6()
declare extern_weak void @_tim7()
declare extern_weak void @_dma2_channel1()
declare extern_weak void @_dma2_channel2()
declare extern_weak void @_dma2_channel3()
declare extern_weak void @_dma2_channel4_5()

@__vectors__ = appending global [60 x i32*] [
  i32* bitcast(void()* @_wwdg to i32*),
  i32* bitcast(void()* @_pvd to i32*),
  i32* bitcast(void()* @_tamper to i32*),
  i32* bitcast(void()* @_rtc to i32*),
  i32* bitcast(void()* @_flash to i32*),
  i32* bitcast(void()* @_rcc to i32*),
  i32* bitcast(void()* @_exti0 to i32*),
  i32* bitcast(void()* @_exti1 to i32*),
  i32* bitcast(void()* @_exti2 to i32*),
  i32* bitcast(void()* @_exti3 to i32*),
  i32* bitcast(void()* @_exti4 to i32*),
  i32* bitcast(void()* @_dma1_channel1 to i32*),
  i32* bitcast(void()* @_dma1_channel2 to i32*),
  i32* bitcast(void()* @_dma1_channel3 to i32*),
  i32* bitcast(void()* @_dma1_channel4 to i32*),
  i32* bitcast(void()* @_dma1_channel5 to i32*),
  i32* bitcast(void()* @_dma1_channel6 to i32*),
  i32* bitcast(void()* @_dma1_channel7 to i32*),
  i32* bitcast(void()* @_adc1_2 to i32*),
  i32* bitcast(void()* @_usb_hp_can_tx to i32*),
  i32* bitcast(void()* @_usb_lp_can_rx0 to i32*),
  i32* bitcast(void()* @_can_rx1 to i32*),
  i32* bitcast(void()* @_can_sce to i32*),
  i32* bitcast(void()* @_exti9_5 to i32*),
  i32* bitcast(void()* @_tim1_brk_tim9 to i32*),
  i32* bitcast(void()* @_tim1_up_tim10 to i32*),
  i32* bitcast(void()* @_tim1_trg_com_tim11 to i32*),
  i32* bitcast(void()* @_tim1_cc to i32*),
  i32* bitcast(void()* @_tim2 to i32*),
  i32* bitcast(void()* @_tim3 to i32*),
  i32* bitcast(void()* @_tim4 to i32*),
  i32* bitcast(void()* @_i2c1_ev to i32*),
  i32* bitcast(void()* @_i2c1_er to i32*),
  i32* bitcast(void()* @_i2c2_ev to i32*),
  i32* bitcast(void()* @_i2c2_er to i32*),
  i32* bitcast(void()* @_spi1 to i32*),
  i32* bitcast(void()* @_spi2 to i32*),
  i32* bitcast(void()* @_usart1 to i32*),
  i32* bitcast(void()* @_usart2 to i32*),
  i32* bitcast(void()* @_usart3 to i32*),
  i32* bitcast(void()* @_exti15_10 to i32*),
  i32* bitcast(void()* @_rtcalarm to i32*),
  i32* bitcast(void()* @_usbwakeup to i32*),
  i32* bitcast(void()* @_tim8_brk_tim12 to i32*),
  i32* bitcast(void()* @_tim8_up_tim13 to i32*),
  i32* bitcast(void()* @_tim8_trg_com_tim14 to i32*),
  i32* bitcast(void()* @_tim8_cc to i32*),
  i32* bitcast(void()* @_adc3 to i32*),
  i32* bitcast(void()* @_fsmc to i32*),
  i32* bitcast(void()* @_sdio to i32*),
  i32* bitcast(void()* @_tim5 to i32*),
  i32* bitcast(void()* @_spi3 to i32*),
  i32* bitcast(void()* @_uart4 to i32*),
  i32* bitcast(void()* @_uart5 to i32*),
  i32* bitcast(void()* @_tim6 to i32*),
  i32* bitcast(void()* @_tim7 to i32*),
  i32* bitcast(void()* @_dma2_channel1 to i32*),
  i32* bitcast(void()* @_dma2_channel2 to i32*),
  i32* bitcast(void()* @_dma2_channel3 to i32*),
  i32* bitcast(void()* @_dma2_channel4_5 to i32*)
], section ".vectors"

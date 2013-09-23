declare extern_weak void @_wwdg()
declare extern_weak void @_pvd()
declare extern_weak void @_tamper_stamp()
declare extern_weak void @_rtc_wkup()
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
declare extern_weak void @_adc1()
declare extern_weak void @_usb_hp()
declare extern_weak void @_usb_lp()
declare extern_weak void @_dac()
declare extern_weak void @_comp/ca()
declare extern_weak void @_exti9_5()
declare extern_weak void @_lcd()
declare extern_weak void @_tim9()
declare extern_weak void @_tim10()
declare extern_weak void @_tim11()
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
declare extern_weak void @_rtc_alarm()
declare extern_weak void @_usb_fs_wkup()
declare extern_weak void @_tim6()
declare extern_weak void @_tim7()
declare extern_weak void @_tim5()
declare extern_weak void @_spi3()
declare extern_weak void @_dma2_ch1()
declare extern_weak void @_dma2_ch2()
declare extern_weak void @_dma2_ch3()
declare extern_weak void @_dma2_ch4()
declare extern_weak void @_dma2_ch5()
declare extern_weak void @_aes()
declare extern_weak void @_comp_acq()

@__vectors__ = appending global [54 x i32*] [
  i32* bitcast(void()* @_wwdg to i32*),
  i32* bitcast(void()* @_pvd to i32*),
  i32* bitcast(void()* @_tamper_stamp to i32*),
  i32* bitcast(void()* @_rtc_wkup to i32*),
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
  i32* bitcast(void()* @_adc1 to i32*),
  i32* bitcast(void()* @_usb_hp to i32*),
  i32* bitcast(void()* @_usb_lp to i32*),
  i32* bitcast(void()* @_dac to i32*),
  i32* bitcast(void()* @_comp/ca to i32*),
  i32* bitcast(void()* @_exti9_5 to i32*),
  i32* bitcast(void()* @_lcd to i32*),
  i32* bitcast(void()* @_tim9 to i32*),
  i32* bitcast(void()* @_tim10 to i32*),
  i32* bitcast(void()* @_tim11 to i32*),
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
  i32* bitcast(void()* @_rtc_alarm to i32*),
  i32* bitcast(void()* @_usb_fs_wkup to i32*),
  i32* bitcast(void()* @_tim6 to i32*),
  i32* bitcast(void()* @_tim7 to i32*),
  i32* bitcast(void()* @_tim5 to i32*),
  i32* bitcast(void()* @_spi3 to i32*),
  i32* bitcast(void()* @_dma2_ch1 to i32*),
  i32* bitcast(void()* @_dma2_ch2 to i32*),
  i32* bitcast(void()* @_dma2_ch3 to i32*),
  i32* bitcast(void()* @_dma2_ch4 to i32*),
  i32* bitcast(void()* @_dma2_ch5 to i32*),
  i32* bitcast(void()* @_aes to i32*),
  i32* bitcast(void()* @_comp_acq to i32*)
], section ".vectors"

device := maple-leaf

llvm   := -3.4
cross  := arm-linux-gnueabi-

stdrtl := class.fy object.fy value.fy integer.fy fixed.fy \
				  symbol.fy lambda.fy
stdrtl := $(addprefix ../rtl/,$(stdrtl))

devrtl := metal/register.fy metal/unit.fy \
					stm32f1/rcc.fy stm32f1/afio.fy stm32f1/gpio.fy stm32f1/exti.fy \
					stm32f1/usb.fy stm32f1/stm32f1.fy \
					maple_leaf/color_lcd_shield.fy maple_leaf/maple_leaf.fy
devrtl := $(addprefix ../rtl/,$(devrtl))

devlib := startup32.ll cortex-m3.ll stm32f1.ll
devlib := $(addprefix ../lib/startup/,$(devlib))

.SUFFIXES:

# Common rules for all targets

%.fir: $(stdrtl) $(devrtl) %.fy
	../foundry_vm.native $^ -o $@

%.opt.fir: %.fir
	../foundry_xfrm.native $^ -std-xfrms -o $@

%.ll: %.bc
	llvm-dis$(llvm) $^ -o $@

%.o: %.opt.bc
	llc$(llvm) -filetype=obj $^ -o $@

%.s: %.opt.bc
	llc$(llvm) -filetype=asm $^ -o $@

%.bin: %.elf
	$(cross)objcopy -Obinary $^ $@

clean:
	rm -f *.fir *.bc *.ll *.s *.o *.bin *.elf

# (h)ost rules

%.h.bc: %.opt.fir
	../foundry_gen.native -target x86_64 $^ -o $@

%.h.opt.bc: %.h.bc
	opt$(llvm) $^ \
		-std-link-opts \
		-o $@

%.h.elf: %.h.o
	gcc $^ -o $@

# (d)evice rules

%.d.bc: %.opt.fir
	../foundry_gen.native -target thumbv7m $^ -o $@

%.d.opt.bc: $(devlib) %.d.bc
	llvm-link$(llvm) $^ | opt$(llvm) \
		-internalize-public-api-list=_startup,__vectors__ \
		-internalize \
		-Os \
		-globaldce \
		-o $@

%.d.elf: %.d.o
	$(cross)ld -L../lib -Tdevices/$(device).ld $^ -o $@


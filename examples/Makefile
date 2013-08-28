device := stm32f100rb

llvm   := -3.4
cross  := arm-linux-gnueabi-

stdlib := startup32.ll cortex-m3.ll volatile.ll
stdlib := $(addprefix ../lib/,$(stdlib))

.SUFFIXES:

# Common rules for all targets

%.fir: %.fy
	../foundry_vm.native $^ -o $@

%.opt.fir: %.fir
	../foundry_xfrm.native $^ -std-xfrms -o $@

%.o: %.opt.bc
	llc$(llvm) -filetype=obj $^ -o $@

%.s: %.opt.bc
	llc$(llvm) -filetype=asm $^ -o $@

clean:
	rm -f *.fir *.bc *.s *.o *.bin *.elf

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

%.d.opt.bc: $(stdlib) %.d.bc
	llvm-link$(llvm) $^ | opt$(llvm) \
		-internalize-public-api-list=_startup,__vectors__ \
		-internalize \
		-std-compile-opts \
		-globaldce \
		-o $@

%.d.elf: %.d.o
	$(cross)ld -L../lib -T$(device).ld $^ -o $@

%.d.bin: %.d.elf
	$(cross)objcopy -Obinary $^ $@

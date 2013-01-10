run: test
	./test

dis: test.bc
	llvm-dis-3.2 $^ -o -

objdump: test.o
	objdump -d $^

clean:
	@rm -f *.bc *.ll *.o test

%.bc: %.rb
	../bin/foundry $^ -o int-$@
	llvm-link-3.2 int-$@ ../rtl/testbench.ll -o link-$@
	opt-3.2 link-$@ -std-link-opts -o $@

%.o: %.bc
	llc-3.2 -filetype=obj $^ -o $@

%: %.o
	gcc $^ -o $@
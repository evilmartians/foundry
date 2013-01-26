run: test
	./test

dis: test.rb
	../bin/foundry $^ -LS

objdump: test.o
	llvm-objdump-3.2 -d $^

%.o: %.rb
	../bin/foundry $^ -o $@

%: %.o
	gcc $^ -o $@

clean:
	@rm -f *.o test

run: test
	./test

dis: test.rb
	../bin/foundry $^ -LS

objdump: test.o
	objdump -g -d $^

%.o: %.rb
	../bin/foundry $^ -o $@

%: %.o
	gcc -g $^ -o $@

clean:
	@rm -f *.o test

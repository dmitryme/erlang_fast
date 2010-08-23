all:
	mkdir -p ebin && erl -make

test:
	./run_tests ebin

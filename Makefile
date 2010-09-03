all:
	mkdir -p ebin && erl -make
	./run_tests ebin

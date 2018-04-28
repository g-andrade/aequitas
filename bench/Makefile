.PHONY: build quick full

build:
	mix deps.get
	mix deps.compile

run: build
	mix run bench/bench.exs quickest html

clean:
	rm -rf mix.lock _build deps

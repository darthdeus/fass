default: build run clean

build:
	ghc -o fass src/Main.hs -H64m -Wall -isrc

run:
	./fass

clean:
	rm -f $(shell find . -name '*.hi')
	rm -f $(shell find . -name '*.o')

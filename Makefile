Tokenizer: Main.o Tokenizer.o
	ghc Main.o Tokenizer.o -o main

Main.o: Main.hs
	ghc -c --make Main.hs

clean: 
	rm *.o *.hi
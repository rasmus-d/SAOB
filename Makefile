Tokenizer: Main.o Tokenizer.o Parser.o
	ghc Main.o Tokenizer.o Parser.o -o main

Main.o: Main.hs
	ghc -c --make Main.hs

clean: 
	rm *.o *.hi
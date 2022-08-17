Tokenizer: Main.o Parser.o
	ghc Main.o Parser.o -o main

Main.o: Main.hs
	ghc -c --make Main.hs

clean: 
	rm *.o *.hi
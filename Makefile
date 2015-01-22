COMP=ghc
LEX=alex

lexerdriver : lexerdriver.hs tigerlexer.hs
	$(COMP) lexerdriver

tigerlexer.hs : tigerlexer.x
	$(LEX) -g tigerlexer.x

clean :
	rm -f lexerdriver.o lexerdriver tigerlexer.o tigerlexer.hs

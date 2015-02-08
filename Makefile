COMP=ghc
LEX=alex

all : lexerdriver parserdriver semantdriver

lexerdriver : lexerdriver.hs tigerlexer.hs
	$(COMP) lexerdriver

parserdriver : parserdriver.hs tigerlexer.hs
	$(COMP) parserdriver

semantdriver : semantdriver.hs tigerlexer.hs
	$(COMP) semantdriver

tigerlexer.hs : tigerlexer.x
	$(LEX) -g tigerlexer.x

clean :
	rm -f *.o *.hi tigerlexer.hs lexerdriver parserdriver semantdriver

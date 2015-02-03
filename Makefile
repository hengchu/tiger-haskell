COMP=ghc
LEX=alex

all : lexerdriver parserdriver sementdriver

lexerdriver : lexerdriver.hs tigerlexer.hs
	$(COMP) lexerdriver

parserdriver : parserdriver.hs tigerlexer.hs
	$(COMP) parserdriver

sementdriver : sementdriver.hs tigerlexer.hs
	$(COMP) sementdriver

tigerlexer.hs : tigerlexer.x
	$(LEX) -g tigerlexer.x

clean :
	rm -f *.o *.hi tigerlexer.hs lexerdriver parserdriver sementdriver

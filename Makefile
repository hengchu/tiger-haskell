COMP=ghc
LEX=alex

all : lexerdriver parserdriver sementdriver

lexerdriver : lexerdriver.hs tigerlexer.hs
	$(COMP) lexerdriver

parserdriver : parserdriver.hs
	$(COMP) parserdriver

sementdriver : sementdriver.hs
	$(COMP) sementdriver

tigerlexer.hs : tigerlexer.x
	$(LEX) -g tigerlexer.x

clean :
	rm -f lexerdriver.o lexerdriver tigerlexer.o tigerlexer.hs parserdriver sementdriver

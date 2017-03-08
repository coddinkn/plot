GHC=ghc
LEX=alex
YACC=happy

all: plotc Test

.PHONY: clean

plotc: src/Main.hs src/Lexer.hs src/Parser.hs
	$(GHC) $^ -o plotc
	cp src/Lexer.hs src/Parser.hs test
	rm -f src/*.o src/*.hi

Test: test/Test.hs test/TestParser.hs test/Lexer.hs test/Parser.hs
	$(GHC) $^ -o Test
	rm -f test/*.o test/*.hi

src/Lexer.hs: src/Lexer.x
	$(LEX) $<

src/Parser.hs: src/Parser.y
	$(YACC) $<

clean:
	rm -f plotc src/*.o src/*.hi
	rm -f Test test/*.o test/*.hi

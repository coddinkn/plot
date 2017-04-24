GHC=ghc
LEX=alex
YACC=happy

all: ramblec

.PHONY: clean

ramblec: src/Main.hs src/Lexer.hs src/Parser.hs src/Expr.hs
	$(GHC) $^ -o $@
	rm -f src/*.o src/*.hi

tests: test/Test.hs test/TestParser.hs test/Lexer.hs test/Parser.hs test/Expr.hs
	$(GHC) $^ -o $@
	rm -f test/*.o test/*.hi

test/Lexer.hs: src/Lexer.hs
	cp src/Lexer.hs test

test/Parser.hs: src/Parser.hs
	cp src/Parser.hs test

test/Expr.hs: src/Expr.hs
	cp src/Expr.hs test

src/Lexer.hs: src/Lexer.x
	$(LEX) $<

src/Parser.hs: src/Parser.y 
	$(YACC) -i $<
	$(YACC) $<

clean:
	rm -f ramblec src/*.o src/*.hi src/*.info
	rm -f tests test/*.o test/*.hi

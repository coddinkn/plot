import Lexer
import Parser

front_end = Parser.parse . Lexer.lex

main :: IO()
main = do
    string <- getContents
    print $ front_end string

import Lexer  (tokenize)
import Parser (parse)

front = parse . tokenize

main :: IO()
main = do
    input <- getContents
    print $ front input

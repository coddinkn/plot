import Lexer
import Parser
import Expr

main :: IO ()
main = do
    line <- getLine
    if null line then 
      return ()
    else do 
      putStrLn . pretty_print_expr . cps_expr . parse . tokenize $ line
      main    

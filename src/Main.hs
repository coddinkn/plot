import Lexer
import Parser

autoCurry :: Expr -> Expr
autoCurry expr = 
  case expr of
    ELambda (p:prms) body -> 
      ELambda [p] $ autoCurry $ ELambda prms body
    ELambda [] body -> 
      autoCurry body

    EApp func (arg:args) -> 
      autoCurry $ EApp (EApp func [arg]) args
    EApp func [] -> 
      func 

    e -> e

front = parse . tokenize

main :: IO()
main = do
    input <- getContents
    print $ (autoCurry . front) input

import Lexer
import Parser
import Expr

autoCurry :: Expr -> Expr
autoCurry expr = 
  case expr of
    ELambda (p:prms) body -> 
      ELambda [p] $ autoCurry $ ELambda prms body
    ELambda [] body ->
      autoCurry body
    EApp func (arg:args) -> 
      autoCurry $ EApp (EApp (autoCurry func) [arg]) args
    EApp func [] -> 
      func 
    e -> e

cps :: Expr -> Expr
cps expr = 
  case expr of
    EId  x -> ELambda [(EId "k")] (EApp (EId "k") [expr])
    e -> e

main :: IO ()
main = do
    line <- getLine
    if null line then 
      return ()
    else do 
      print . cps . autoCurry . parse . tokenize $ line
      main    



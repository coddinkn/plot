module Expr where

import Data.List

data Expr a = ELam [Expr a] (Expr a) a
            | EApp [Expr a] a
            | EId  String a
            | EInt Int a
            deriving (Eq, Show)

cps_expr :: Expr a -> Expr ()
cps_expr expr = 
  let cont e = ELam [EId "k" ()] (EApp ((EId "k" ()):[e]) ()) ()
  in case expr of
    EInt num _ -> 
      cont $ EInt num ()

    EId name _ ->
      cont $ EId name () 

    ELam params body _ ->
      cont (ELam ((EId "k" ()):(map cps_expr params)) 
             (EApp ((cps_expr body):[EId "k" ()]) ()) ())

    EApp (function:args) _ ->
      let cpsd_args = foldl (\e a -> 
                              (EApp [(cps_expr a), (ELam [EId "$" ()] e ())]) ())
                            (EApp ((map (\_ -> EId "$" ()) args) ++ [EId "k" ()]) ())
                            args
      in ELam [EId "k" ()] cpsd_args ()

pretty_print_expr :: Expr a -> String
pretty_print_expr = print_spaced_expr 0

print_spaced_expr :: Int -> Expr a -> String
print_spaced_expr indent expr =
  let spaces = take indent $ repeat ' '
  in case expr of
      ELam params body _ -> 
         let gap = length $ print_expr $ last params             
         in spaces 
         ++ "{" 
         ++ (dropWhile (== ' ') $ concat $ intersperse "\n " $ map (print_spaced_expr indent) $ init params)
         ++ (print_expr $ last params)
         ++ " ->\n" 
         ++ (print_spaced_expr (indent + gap) body) 
         ++ "}"

      EApp exprs _ -> 
        let function = print_expr $ head exprs 
            gap = (+) 2 $ length $ function
            args = map (print_spaced_expr gap) $ tail exprs
        in spaces
        ++ "(" 
        ++ function
        ++ " "
        ++ (dropWhile ((==) ' ') $ concat $ intersperse "\n" args) 
        ++ ")"

      EId name _ -> spaces 
                 ++ name
      EInt num _ -> spaces
                 ++ show num
 
print_expr :: Expr a -> String
print_expr expr =
  case expr of
    ELam params body _ -> "{" 
                       ++ (concat $ intersperse " " $ map print_expr params) 
                       ++ " -> " 
                       ++ (print_expr body) 
                       ++ "}"

    EApp exprs _ -> "(" 
                 ++ (concat $ intersperse " " $ map print_expr exprs) 
                 ++ ")"

    EId name _ -> name
    EInt num _ -> show num

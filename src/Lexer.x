{
module Lexer where
}

%wrapper "basic"

$digit = 0-9              -- digits
$alpha = [a-zA-Z]         -- alphabetic characters
$op    = [\-\+\*\:\/\^\%] -- operators

tokens :-
  \(      { \s -> TLP }
  \)      { \s -> TRP }
  \{      { \s -> TLB }
  \}      { \s -> TRB }
  let     { \s -> TLet }
  \-\>    { \s -> TArr }
  $op     { \s -> TOp $ strToOp s }
  def     { \s -> TDef }
  rec     { \s -> TRec }
  $digit+ { \s -> TInt $ read s }
  $alpha+ { \s -> TId s }
  $white+ ;

{
data Op = Plus
        | Minus
        | Times
        | Power
        | Mod
        | Cons
        | Divide
        deriving (Eq, Show)

strToOp :: String -> Op
strToOp s = case s of "+" -> Plus
                      "-" -> Minus
                      "*" -> Times
                      "/" -> Divide
                      "^" -> Power
                      ":" -> Cons
                      "%" -> Mod

data Token = TId String
           | TInt Int
           | TOp Op
           | TRP
           | TLP
           | TLB
           | TRB
           | TArr
           | TApp
           | TLet
           | TDef 
           | TRec
           deriving (Eq, Show)

type Tokens = [Token]

tokenize :: String -> Tokens
tokenize = alexScanTokens
}

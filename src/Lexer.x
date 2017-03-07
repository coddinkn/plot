{
module Lexer (lex, Token(..)) where
import Prelude hiding (lex)
}

%wrapper "basic"

$digit = 0-9              -- digits
$alpha = [a-zA-Z]         -- alphabetic characters
$op    = [\-\+\*\:\/\^\%] -- operators

tokens :-
  \(      { \s -> TLParen }
  \)      { \s -> TRParen }
  let     { \s -> TLet }
  \-\>    { \s -> TLambda }
  $op     { \s -> TOp s }
  def     { \s -> TDef }
  rec     { \s -> TRec }
  $digit+ { \s -> TInt (read s) }
  $alpha+ { \s -> TId s }
  $white+ ;

{
-- The token type
data Token = TId String
           | TInt Int
           | TOp String
           | TRParen
           | TLParen
           | TLambda
           | TLet
           | TDef 
           | TRec
           deriving (Eq,Show)

lex :: String -> [Token]
lex = alexScanTokens
}

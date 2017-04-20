{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      id   { TId  $$ }
      '->' { TArrow  }
      '('  { TLParen }
      ')'  { TRParen }
      lam  { TLambda }

%%

expr  : '(' lam exprs '->' expr ')' { ELambda $3 $5 }
      | '(' expr exprs ')'          { EApp $2 $3 }
      | id                          { EId  $1 }

exprs : {- empty -}  { [] }
      | expr exprs   { $1 : $2 }

{
parseError :: Tokens -> a
parseError _ = error "Parse error"

data Expr = ELambda [Expr] Expr
          | EApp    Expr [Expr]
          | EId     String
          | EInt    Int
          deriving (Eq, Show)
}


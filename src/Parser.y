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
      | exprs expr   { $2 : $1 }

{
parseError :: Tokens -> a
parseError _ = error "Parse error"

data Expr = ELambda [Expr] Expr
          | EApp    Expr [Expr]
          | EId     String
          deriving (Eq, Show)
}


{
module Parser where
import Lexer
import Expr
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      id   { TId  $$ }
      int  { TInt $$ }
      '->' { TArr }
      '('  { TLP }
      ')'  { TRP }
      '{'  { TLB }
      '}'  { TRB }

%%

expr  : '{' exprs '->' expr '}' { ELam $2 $4 () }
      | '(' exprs ')'           { EApp $2 () }
      | id                      { EId  $1 () }
      | int                     { EInt $1 () }

exprs : {- empty -}  { [] }
      | expr exprs   { $1 : $2 }

{
parseError :: Tokens -> a
parseError _ = error "Parse error"
}


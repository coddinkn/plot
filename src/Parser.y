{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      let    { TLet }
      int    { TInt $$ }
      id     { TId  $$ }
      op     { TOp  $$ }
      lambda { TLambda }
      '('    { TLParen }
      ')'    { TRParen }
      def    { TDef }
      rec    { TRec }

%%

expr  : '(' ids lambda expr ')' { ELambda $2 $4 }
      | '(' id exprs ')'        { EApp    $2 $3 }
      | id                      { EId  $1 }

exprs : {- empty -}  { [] }
      | exprs expr   { $2 : $1 }

ids   : {- empty -}  { [] }
      | id ids       { $1 : $2 }

{
parseError :: Tokens -> a
parseError _ = error "Parse error"

type Id = String

data Expr = ELambda [Id] Expr
          | EBinOp  Expr Op Expr
          | EApp    Id [Expr]
          | EId     Id
          | EInt    Int
          deriving (Eq,Show)
}


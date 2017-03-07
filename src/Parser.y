{
module Parser (parse, Expr(..)) where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      let    { TLet }
      int    { TInt $$ }
      id     { TId $$ }
      op     { TOp $$ }
      lambda { TLambda }
      '('    { TLParen }
      ')'    { TRParen }
      def    { TDef }
      rec    { TRec }

%%

expr : '(' ids lambda expr ')' { ELambda $2 $4 }
     | '(' expr op expr ')'    { EBinOp  $2 $3 $4 }
     | '(' id exprs ')'        { EApp    $2 $3 }
     | id  { EId  $1 }
     | int { EInt $1 }

exprs : {- empty -}  { [] }
      | exprs expr   { $2 : $1 }

ids : id     { [$1] }
    | ids id { $2 : $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Op = String
type Id = String

data Expr = ELambda [Id] Expr
          | EBinOp  Expr Op Expr
          | EApp    Id [Expr]
          | EId     Id
          | EInt    Int
          deriving (Show)
}

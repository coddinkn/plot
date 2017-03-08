module TestParser (parserTests) where 

import Test.HUnit
import Lexer
import Parser

-- | Test Parse Tokens
tpt :: Int -> Expr -> Tokens -> Test
tpt n expr toks = TestCase $ assertEqual (show n) expr $ parse toks

parserTests = TestLabel "parse_tokens" $ 
              TestList  [ tpt 1 (EId  "x")               [ TId  "x" ] 
                        , tpt 2 (ELambda ["x"] (EInt 5)) [ TLParen
                                                         , TId "x"
                                                         , TLambda
                                                         , TInt 5
                                                         , TRParen 
                                                         ]
                        , tpt 3 (EApp "fun" [])          [ TLParen
                                                         , TId "fun"
                                                         , TRParen
                                                         ]
                        ]

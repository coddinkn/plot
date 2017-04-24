module TestParser (parserTests) where 

import Test.HUnit
import Lexer
import Parser
import Expr

-- | Test Parse Tokens
tpt :: Int -> Expr -> Tokens -> Test
tpt n expr toks = TestCase $ assertEqual (show n) expr $ parse toks

parserTests = TestLabel "parse_tokens" $ 
              TestList  []

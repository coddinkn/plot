import Test.HUnit
import TestParser

tests = parserTests

main :: IO ()
main = do
    runTestTT tests
    return ()

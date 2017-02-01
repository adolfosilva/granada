import           Test.Tasty

import qualified Granada.Tests.Parser as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [Parser.propTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [Parser.unitTests]


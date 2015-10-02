{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad (void)
import           Test.HUnit (Test(..), (~=?), runTestTT)

import           Zoli.Pattern

patternTests :: Test
patternTests = TestLabel "Zoli.Pattern" $ TestList
  [ TestLabel "pat2 instantiate" $
      "blah.o" ~=? patInstantiate pat2_test "blah"
  , TestLabel "pat2 match" $
      Just "blah" ~=? patMatch pat2_test "blah.o"
  ]
  where
    pat2_test = pat2 "" ".o"

main :: IO ()
main = do
  void $ runTestTT $ TestList [patternTests]

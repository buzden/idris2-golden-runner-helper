module Runner

import BaseDir

import Test.Golden.RunnerHelper

CmdUnderTest where
  cmdUnderTest = baseTestsDir ++ "/.pack_lock"

main : IO ()
main = goldenRunner
  [ "Simple tests" `atDir` "simple-tests"
  , "Tests that use Hedgehog" `atDir` "using-hedgehog"
  ]

module Test.Golden.RunnerHelper

import Data.Maybe
import Data.String

import public Test.Golden

import System
import System.Directory

--- Configuration facilities ---

public export
interface BaseTestsDir where
  constructor MkBaseTestsDir
  baseTestsDir : String

||| Determines which string will be passed as the first argument
||| to the `run` script of each test.
public export
interface RunScriptArg where
  constructor MkRunScriptArg
  runScriptArg : String

public export
%defaulthint
DefaultRunScriptArg : RunScriptArg
DefaultRunScriptArg = MkRunScriptArg "idris2"

--- Options management ---

nproc : IO $ Maybe Nat
nproc = do
  (str, 0) <- run "nproc"
    | _ => pure Nothing
  pure $ parsePositive str

nproc' : IO Nat
nproc' = fromMaybe 1 . filter (> 0) <$> nproc

fitsPattern : (pattern, test : String) -> Bool
fitsPattern = isInfixOf

testOptions : RunScriptArg => IO Options
testOptions = do
  onlies <- filter (not . null) . tail' <$> getArgs
  pure $
    { color := isNothing !(getEnv "NO_COLOR")
    , timing := True
    , interactive := !((Just "true" /=) <$> getEnv "CI")
    , failureFile := Just "failures"
    , onlyNames := onlies <&> \patterns, test => any (`fitsPattern` test) patterns
    , threads := !nproc'
    } (initOptions runScriptArg True)

--- A universal way to set test pools from different origins ---

export
interface TestPoolLike a where
  toTestPool : a -> IO TestPool

export
TestPoolLike (IO TestPool) where
  toTestPool = id

export
TestPoolLike TestPool where
  toTestPool = pure

export
data TestPools = MkTestPools (List $ IO TestPool)

namespace TestPools

  export
  Nil : TestPools
  Nil = MkTestPools []

  export
  (::) : TestPoolLike a => a -> TestPools -> TestPools
  x :: MkTestPools xs = MkTestPools $ toTestPool x :: xs

toList : TestPools -> List $ IO TestPool
toList $ MkTestPools xs = xs

--- Facilities for user's convenience ---

export
atDir : (poolName : String) -> (dir : String) -> IO TestPool
atDir poolName dir = do
  True <- exists dir
    | False => emptyPool
  Right (_::_) <- listDir dir
    | _ => emptyPool
  testsInDir dir (not . isPrefixOf "_") poolName [] Nothing

  where
    emptyPool : IO TestPool
    emptyPool = pure $ MkTestPool poolName [] Nothing []

--- Toplevel running ---

export
goldenRunner : RunScriptArg => BaseTestsDir => TestPools -> IO ()
goldenRunner tps = do
  ignore $ changeDir baseTestsDir
  runnerWith !testOptions !(sequence $ toList tps)

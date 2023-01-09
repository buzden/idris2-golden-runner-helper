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

||| When no default argument is given, is passes a filename for "pack lock",
||| a file to be locked over when running `pack -q install-deps test.ipkg` using `flock`.
||| This is most useful when testing libraries, this only `pack` or `idris2` commands are used in tests.
public export
%defaulthint
DefaultRunScriptArg : BaseTestsDir => RunScriptArg
DefaultRunScriptArg = MkRunScriptArg $ baseTestsDir ++ "/.pack_lock"

--- Options management ---

nproc : IO $ Maybe Nat
nproc = do
  rawThreads <- getEnv "NUM_THREADS"
  let Nothing = rawThreads >>= parsePositive
    | Just n => pure $ Just n
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
  toTestPool : a -> IO $ List TestPool

export
TestPoolLike (IO TestPool) where
  toTestPool = map pure

export
TestPoolLike TestPool where
  toTestPool = pure @{Compose}

export
TestPoolLike (List TestPool) where
  toTestPool = pure

export
TestPoolLike (IO $ List TestPool) where
  toTestPool = id

export
TestPoolLike (List $ IO TestPool) where
  toTestPool = sequence

export
data TestPools = MkTestPools (IO $ List TestPool)

namespace TestPools

  export
  Nil : TestPools
  Nil = MkTestPools $ pure []

  export
  (::) : TestPoolLike a => a -> TestPools -> TestPools
  x :: MkTestPools xs = MkTestPools [| toTestPool x ++ xs |]

  export
  (++) : TestPools -> TestPools -> TestPools
  MkTestPools xs ++ MkTestPools ys = MkTestPools [| xs ++ ys |]

toList : TestPools -> IO $ List TestPool
toList $ MkTestPools xs = xs

--- Facilities for user's convenience ---

export
atDir : (poolName : String) -> (dir : String) -> IO TestPool
atDir poolName dir = do
  True <- exists dir
    | False => emptyPool
  Right (_::_) <- listDir dir
    | _ => emptyPool
  testsInDir dir poolName {pred=not . isPrefixOf "_"}

  where
    emptyPool : IO TestPool
    emptyPool = pure $ MkTestPool poolName [] Nothing []

--- Toplevel running ---

export
goldenRunner : RunScriptArg => BaseTestsDir => TestPools -> IO ()
goldenRunner tps = do
  ignore $ changeDir baseTestsDir
  runnerWith !testOptions !(toList tps)

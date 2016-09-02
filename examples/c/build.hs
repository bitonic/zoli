import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Class (lift)
import           Data.Functor.Identity (Identity(..))
import           Development.Shake.Command (cmd)
import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.Environment (getArgs)
import           System.FilePath (takeDirectory)
import           Control.Monad.IO.Class (liftIO)

import           Zoli
import           Zoli.Pattern
import           Zoli.Run.Dumb

build :: Pattern tok => Rules tok IO Identity ()
build = do
  objects <- rule (pat2 "objs/" ".o") $ \name out -> do
    let source = pat2 "src/" ".c" @@ name
    needFile source
    let deps = pat2 "deps/" ".d" @@ name
    liftIO $ createDirectoryIfMissing True $ takeDirectory deps
    () <- lift $ cmd "cc -MD -MF" [deps] "-c" "-o" [out] [source]
    depsFiles <- lift $ readFile deps
    forM_ (words depsFiles) needFile

  void $ rule (pat1 "bin/myprog") $ \() out -> do
    let names = ["a", "b"]
    objectsFiles <- mapM (need objects) names
    lift $ cmd "cc" "-o" [out] objectsFiles

  void $ phony "clean" $ liftIO $ do
    removeDirectoryRecursive "objs"
    removeDirectoryRecursive "deps"

main :: IO ()
main = do
  args <- getArgs
  let cfg = Config
        { cfgTmpDir = ".tmp"
        , cfgRunRules = return . runIdentity
        , cfgRunRule = id
        }
  let cmd' = if null args then ShowRules else Build args
  runRules cfg cmd' build

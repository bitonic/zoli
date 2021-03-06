{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Zoli.Run.Dumb
  ( Config(..)
  , Cmd(..)
  , runRules
  ) where


import           Control.Monad (forM, guard, unless)
import           Control.Monad.Trans.Free (FreeF(..), FreeT(..))
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath (takeDirectory, (</>))
import           System.Posix.Files (rename)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Zoli.Pattern
import qualified Zoli.Core as Core

data Token m a where
  RuleToken :: (Pattern f) => f a -> RuleHandler m a -> Token m a
  ExistingToken :: FilePath -> Token m ()
  PhonyToken :: String -> Rule m () -> Token m ()

type RulesF m = Core.RulesF (Token m) m
type Rule m = Core.Rule (Token m) m
type RuleF m = Core.RuleF (Token m)
type RuleHandler m a = Core.RuleHandler (Token m) m a

instance Pattern (Token m) where
  patMatch tok fp = case tok of
    RuleToken pat _handler -> patMatch pat fp
    ExistingToken fp' -> guard $ fp == fp'
    -- TODO normalize path -- e.g. trailing slashes etc.
    PhonyToken str _rule -> guard $ str == fp

  patInstantiate tok x = case tok of
    RuleToken pat _handler -> patInstantiate pat x
    ExistingToken fp -> fp
    PhonyToken str _rule -> str

  patDisplay tok = case tok of
    RuleToken pat _ -> patDisplay pat
    ExistingToken fp -> fp
    PhonyToken str _ -> str

data SomeToken m = forall a. SomeToken (Token m a)

data Config m n = Config
  { cfgTmpDir :: FilePath
  , cfgRunRules :: forall a. m a -> IO a
  , cfgRunRule :: forall a. n a -> IO a
  }

data Cmd
  = Build [String]
  | ShowRules
  deriving (Eq, Show)

-- | Runs a set of rules.  Does not cache anything: re-builds everything
-- each time (even inter build, when rules are invoked, they are always
-- ran).
--
-- * @m@ is the monad where the 'Core.Rules' (the thing defining the
-- list of rules) live.
--
-- * @n@ is the monad where the 'Core.Rule' (individual rules) live.
runRules
  :: forall m n. (Monad m, MonadIO n)
  => Config m n
  -> Cmd
  -> (forall tok. (Pattern tok) => Core.Rules tok n m ())
  -- ^ Rules
  -> IO ()
runRules (Config tmpDir liftM liftN) cmd rules = do
  ((), table0) <- liftM $ goRules [] $ Core.unRules rules
  -- This doesn't really matter, but let's display the rules to the user
  -- in the way he wrote them down.
  let table = reverse table0
  case cmd of
    Build targets -> do
      entries <- forM targets $ \target -> do
        let entries =
              [ Core.need tok x
              | SomeToken tok <- table, Just x <- [patMatch tok target]
              ]
        case entries of
          [] -> fail $ "No rules match target " ++ target ++ "!"
          [entry] -> return entry
          -- TODO display the conflicting rules
          _:_ -> fail $ "Multiple rules match target " ++ target ++ "!"
      -- TODO when we can, parallelize this (see TODO for
      -- 'Core.need').
      liftN $ goRule $ Core.unRule $ sequence_ entries
    ShowRules ->
      mapM_ (\(SomeToken tok) -> putStrLn $ patDisplay tok) table
  where
    goRules :: [SomeToken n] -> FreeT (RulesF n) m a -> m (a, [SomeToken n])
    goRules table m = do
      res <- runFreeT m
      case res of
        Pure x -> return (x, table)
        Free r -> case r of
          Core.Existing path cont -> do
            let tok = ExistingToken path
            goRules (SomeToken tok : table) $ cont tok
          Core.Phony s rule cont -> do
            let tok = PhonyToken s rule
            goRules (SomeToken tok : table) $ cont tok
          Core.AddRule pat handler cont -> do
            let tok = RuleToken pat handler
            goRules (SomeToken tok : table) $ cont tok

    goRule :: FreeT (RuleF n) n a -> n a
    goRule m = do
      res <- runFreeT m
      case res of
        Pure x -> return x
        Free rule -> case rule of
          Core.Always cont -> goRule cont
          Core.NeedFile _fp cont -> goRule cont
          Core.Need tok x cont -> do
            case tok of
              ExistingToken fp -> do
                liftIO $ putStrLn $ "# Requesting " ++ fp
                exists <- liftIO $ doesFileExist fp
                unless exists $ do
                  fail $ "File " ++ fp ++ " does not exist."
              PhonyToken str rule' -> do
                liftIO $ putStrLn $ "# Building phony " ++ show str
                goRule $ Core.unRule rule'
              RuleToken pat handler -> do
                let fp = pat @@ x
                liftIO $ putStrLn $ "# Building " ++ show fp
                let tmp = tmpDir </> fp
                liftIO $ createDirectoryIfMissing True $ takeDirectory tmp
                goRule $ Core.unRule $ handler x tmp
                -- TODO check if file was indeed written to and if it's
                -- not print nice error
                liftIO $ createDirectoryIfMissing True $ takeDirectory fp
                liftIO $ rename tmp fp
            goRule cont

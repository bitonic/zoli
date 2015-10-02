{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Zoli.Core
  ( -- * 'Rule'
    Rule
  , unRule
  , RuleF(..)
    -- ** Creating rules
  , ifChange
  , ifChangeFile
  , stamp
  , always
    -- * Rules
  , Rules
  , unRules
  , RulesF(..)
    -- ** Adding rules
  , OutPath
  , RuleHandler
  , rule
  , phony
    -- ** Referring to source files
  , existing
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Free (FreeT, wrap)
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.ByteString as BS

import           Zoli.Pattern

-- Rule
------------------------------------------------------------------------

newtype Rule tok m a = Rule {unRule :: FreeT (RuleF tok) m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

data RuleF tok a
  = forall p. IfChange (tok p) p a
  | IfChangeFile FilePath a
  | Stamp BS.ByteString a
  | Always a

instance Functor (RuleF tok) where
  fmap f (IfChange tok p x) = IfChange tok p (f x)
  fmap f (IfChangeFile p x) = IfChangeFile p (f x)
  fmap f (Stamp bs x) = Stamp bs (f x)
  fmap f (Always x) = Always (f x)

-- Rule building

ifChange
  :: (Monad m, Pattern tok) => tok a -> a -> Rule tok m ()
ifChange tok pat =
  Rule $ wrap $ IfChange tok pat $ return ()

ifChangeFile :: (Monad m) => FilePath -> Rule tok m ()
ifChangeFile fp = Rule $ wrap $ IfChangeFile fp $ return ()

-- | Run this rule when the provided 'BS.ByteString' changes.
stamp :: (Monad m) => BS.ByteString -> Rule tok m ()
stamp st = Rule $ wrap $ Stamp st $ return ()

-- | Always run this rule.
always :: (Monad m) => Rule tok m ()
always = Rule $ wrap $ Always $ return ()

-- Rules
------------------------------------------------------------------------

newtype Rules tok (r :: * -> *) m a = Rules {unRules :: FreeT (RulesF tok r) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

data RulesF tok r a
  = Existing FilePath (tok () -> a)
  | Phony String (Rule tok r ()) (tok () -> a)
  | forall f p. (Pattern f) => AddRule (f p) (RuleHandler tok r p) (tok p -> a)

instance Functor (RulesF tok r) where
  fmap f (Existing p h) = Existing p (fmap f h)
  fmap f (Phony s r h) = Phony s r (fmap f h)
  fmap f (AddRule pat rh h) = AddRule pat rh (fmap f h)

-- Adding rules

-- | The tmp filepath where we need to write the target.  It will then
-- be copied to the 'TargetFile'.
type OutPath = FilePath

type RuleHandler tok r a = a -> OutPath -> Rule tok r ()

-- | Add a given rule to the build process.
rule
  :: (Monad m, Pattern tok, Pattern f)
  => f a -> RuleHandler tok r a -> Rules tok r m (tok a)
rule pat rh = Rules $ wrap $ AddRule pat rh return

phony :: (Monad m, Pattern tok) => String -> Rule tok r () -> Rules tok r m (tok ())
phony s h = Rules $ wrap $ Phony s h return

-- | Refer to an existing file.
--
-- IMPORTANT: The intended use for 'existing' is only for which which
-- exist *before* the build process starts.  Every file created as part
-- of the build process should be referred to using 'Token's returned by
-- 'rule'.
existing :: (Monad m, Pattern tok) => FilePath -> Rules tok r m (tok ())
existing p = Rules $ wrap $ Existing p return

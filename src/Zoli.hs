module Zoli
  ( -- * 'Rule'
    Core.Rule
    -- ** Creating rules
  , need
  , Core.needFile
  , Core.always
    -- * Rules
  , Core.Rules
    -- ** Adding rules
  , Core.OutPath
  , Core.RuleHandler
  , Core.rule
  , Core.phony
    -- ** Referring to source files
  , Core.existing
  ) where

import qualified Zoli.Core as Core
import           Zoli.Pattern

-- | Run this rule when the provided 'Token' or any of its dependencies
-- change.
--
-- Returns the filepath that the applied token represents.
need :: (Monad m, Pattern tok) => tok a -> a -> Core.Rule tok m FilePath
need tok pat = do
  Core.need tok pat
  return (tok @@ pat)

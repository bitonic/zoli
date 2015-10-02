module Zoli
  ( -- * 'Rule'
    Rule
    -- ** Creating rules
  , ifChange
  , ifChangeFile
  , stamp
  , always
    -- * Rules
  , Rules
    -- ** Adding rules
  , OutPath
  , RuleHandler
  , rule
  , phony
    -- ** Referring to source files
  , existing
  ) where

import           Zoli.Core

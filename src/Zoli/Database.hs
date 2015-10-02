module Zoli.Database
  ( Handle
  , init
  , lookupLastSeen
  , putLastSeen
  ) where

import           Prelude hiding (init)

import           Path (Path, Rel, File)

import           Zoli.Timestamp

data Handle

init :: IO Handle
init = error "TODO"

lookupLastSeen :: Handle -> Path Rel File -> IO Timestamp
lookupLastSeen = error "TODO"

putLastSeen :: Handle -> Path Rel File -> Timestamp -> IO Timestamp
putLastSeen = error "TODO"

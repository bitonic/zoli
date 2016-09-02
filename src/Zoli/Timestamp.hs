{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zoli.Timestamp
  ( fileTimestamp
  ) where

import           Control.Exception (tryJust)
import           Control.Monad (guard)
import           Data.Time.Clock.POSIX (POSIXTime)
import           System.IO.Error (isDoesNotExistError)
import           System.Posix.Files (getFileStatus, accessTimeHiRes)

-- | 'Nothing' if the file does not exist.
fileTimestamp :: FilePath -> IO (Maybe POSIXTime)
fileTimestamp path = do
  mbStat <- tryJust (guard . isDoesNotExistError) (getFileStatus path)
  return $ case mbStat of
    Left _err -> Nothing
    Right stat -> Just (accessTimeHiRes stat)

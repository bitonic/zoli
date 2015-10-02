{-# LANGUAGE GADTs #-}
module Zoli.Pattern
  ( Pattern(..)
  , (@@)
  , Pat
  , pat1
  , pat2

  , filePathPattern
  ) where

import           Data.Typeable (Typeable)
import           Data.List (isPrefixOf)
import           Control.Monad (guard)

-- |
-- * If @'patInstantiate' pat x = s@, then @'patMatch' pat s = Just x@
class Pattern f where
  patMatch :: f a -> String -> Maybe a
  patInstantiate :: f a -> a -> String
  -- | This function is just used for debugging and related tasks.  It
  -- should explain in 'String' form what the pattern does in a single
  -- line (e.g. in faux regex syntax).
  patDisplay :: f a -> String

(@@) :: Pattern f => f a -> a -> String
(@@) = patInstantiate

data Pat a where
  Pat1 :: String -> Pat ()
  Pat2 :: String -> String -> Pat String
  deriving (Typeable)

instance Pattern Pat where
  patMatch pat s = case pat of
    Pat1 s' -> guard $ s == s'
    Pat2 s1 s2 -> do
      guard $ s1 `isPrefixOf` s
      guard $ reverse s2 `isPrefixOf` reverse s
      guard $ length s >= length s1 + length s2
      return $ dropEnd (length s2) $ drop (length s1) $ s

  patInstantiate (Pat1 s) () = s
  patInstantiate (Pat2 s1 s2) s = s1 ++ s ++ s2

  patDisplay (Pat1 s) = s
  patDisplay (Pat2 s1 s2) = s1 ++ "*" ++ s2

pat1 :: String -> Pat ()
pat1 = Pat1

pat2 :: String -> String -> Pat String
pat2 = Pat2

dropEnd :: Int -> String -> String
dropEnd n = reverse . drop n . reverse

filePathPattern
  :: (Pattern f, Typeable (f a)) => String -> f a
filePathPattern = error "TODO"

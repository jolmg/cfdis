{-# LANGUAGE FlexibleInstances #-}

module CFDI.Chainable
  ( Chainable(..)
  ) where

import Data.Ratio          (denominator, numerator)
import Data.Text           (Text, append, cons, drop, length, pack)
import Data.Time.Calendar  (Day, showGregorian)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime)
import Numeric             (fromRat, showFFloat)
import Prelude      hiding (drop, length)

class Chainable a where
  chain :: a -> Text

  infixr 9 <@>, <@@>, <~>, <~~>

  -- Chain starters
  (<@>) :: Chainable b => (a -> b) -> (a, Text) -> Text
  (<@>) f = drop 1 . snd . (f <~>)

  (<@@>) :: Chainable b => (a -> [b]) -> (a, Text) -> Text
  (<@@>) f = drop 1 . snd . (f <~~>)

  -- Chain connectors
  (<~>) :: Chainable b => (a -> b) -> (a, Text) -> (a, Text)
  f <~> (x, s) = (x, s' `append` s)
    where
      s'  = if length s'' > 0 then cons '|' s'' else ""
      s'' = chain $ f x

  (<~~>) :: Chainable b => (a -> [b]) -> (a, Text) -> (a, Text)
  f <~~> (x, s) = (x, s' `append` s)
    where
      s' = foldl append "" . map (cons '|' . chain) $ f x

instance Chainable Day where
  chain = pack . showGregorian

instance Chainable Float where
  chain = pack . show

instance Chainable Int where
  chain = pack . show

instance Chainable LocalTime where
  chain = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance Chainable a => Chainable (Maybe a) where
  chain = maybe "" chain

instance Chainable Rational where
  chain r
    | denominator r == 1 = pack . show $ numerator r
    | otherwise = pack $ showFFloat Nothing (fromRat r :: Float) ""

instance Chainable Text where
  chain = id

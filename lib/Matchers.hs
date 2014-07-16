{-# LANGUAGE OverloadedStrings #-}
module Matchers
  ( CaseSensitive(..)
  , pcre
  , within
  , exact
  , anyTime
  , time
  ) where

import Data.Text (Text, pack, toCaseFold, isInfixOf)
import qualified Text.Parsec as P
import qualified Data.Time as Time
import Matchers.Times (dateTime)
import Matchers.Pcre as PCRE
import qualified Prednote as R
import qualified Prednote.Comparisons as R
import Matchers.Types
import Data.Monoid

pcre
  :: CaseSensitive
  -> Text
  -- ^ Pattern
  -> Either String (R.Pred Text)
pcre cs txt = fmap f $ compile cs txt
  where
    f pc = R.predicate st dyn pd
      where
        st = "matches the PCRE regular expression "
          <> txt <> " - " <> s
        s = case cs of
          Sensitive -> "case sensitive"
          Insensitive -> "case insensitive"
        dyn x = "text " <> pack (show x) <> " - " <> st
        pd x = case exec pc x of
          Nothing -> False
          Just b -> b

within
  :: CaseSensitive
  -> Text
  -- ^ Pattern
  -> R.Pred Text
within cs txt = txtMatch isInfixOf st cs txt
  where
    st = "contains the text " <> pack (show txt)

exact
  :: CaseSensitive
  -> Text
  -> R.Pred Text
exact cs txt = txtMatch (==) st cs txt
  where
    st = "exactly matches the text " <> pack (show txt)

txtMatch
  :: (Text -> Text -> Bool)
  -> Text
  -- ^ Static label
  -> CaseSensitive
  -> Text
  -- ^ Pattern
  -> R.Pred Text
txtMatch f lbl c p = R.predicate st dyn pd
  where
    st = lbl <> " - " <> cs
    (cs, flipCase) = case c of
      Sensitive -> ("case sensitive", id)
      Insensitive -> ("case insensitive", toCaseFold)
    dyn txt = "text " <> pack (show txt) <> " - " <> st
    pd t = f pat txt
      where
        txt = flipCase t
        pat = flipCase p


-- | Matches any valid time.
anyTime :: R.Pred Text
anyTime = R.predicate st dyn pd
  where
    st = "is any valid date or time"
    dyn x = "text " <> pack (show x) <> " - " <> st
    pd x = case P.parse dateTime "" x of
      Left _ -> False
      Right _ -> True

-- | If the given ordering is @r@, the given time is @t@, and the
-- time of the subject is @s@, the Predbox returns @compare s t == r@.
-- Always returns False if the subject is not a valid time.
time
  :: Time.UTCTime
  -- ^ @t@
  -> Ordering
  -- ^ @r@
  -> R.Pred Text
time ti ord = R.compareByMaybe "time" descRhs descLhs cmp ord
  where
    descRhs = pack . show $ ti
    descLhs = pack . show
    cmp x = case P.parse dateTime "" x of
      Left _ -> Nothing
      Right g -> Just $ (g `compare` ti)


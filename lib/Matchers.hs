{-# LANGUAGE OverloadedStrings #-}
module Matchers
  ( CaseSensitive(..)
  , pcre
  , within
  , exact
  ) where

import Data.Text (Text, pack, toCaseFold, isInfixOf)
import Matchers.Pcre as PCRE
import qualified Prednote as R
import Data.Monoid

pcre
  :: CaseSensitive
  -> Text
  -- ^ Pattern
  -> Either String (R.Pred Text)
pcre cs txt = fmap f $ compile cs txt
  where
    f pc = R.predicate cond pd
      where
        cond = "matches the PCRE regular expression "
          <> (pack . show $ txt) <> ", " <> s
        s = case cs of
          Sensitive -> "case sensitive"
          Insensitive -> "case insensitive"
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
  -- ^ Condition description
  -> CaseSensitive
  -> Text
  -- ^ Pattern
  -> R.Pred Text
txtMatch f lbl c p = R.predicate cond pd
  where
    cond = lbl <> ", " <> cs
    (cs, flipCase) = case c of
      Sensitive -> ("case sensitive", id)
      Insensitive -> ("case insensitive", toCaseFold)
    pd t = f pat txt
      where
        txt = flipCase t
        pat = flipCase p

module Text.Matchers.Regex.TDFA (tdfa) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception))

import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))


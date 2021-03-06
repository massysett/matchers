{-# LANGUAGE Trustworthy #-}
module Matchers.Pcre
  ( B.CaseSensitive(..)
  , B.PCRE
  , compile
  , exec
  ) where

import qualified Matchers.Pcre.Base as B
import qualified Data.Text as X
import System.IO.Unsafe (unsafePerformIO)

compile
  :: B.CaseSensitive
  -> X.Text
  -> Either String B.PCRE
compile cl x = unsafePerformIO $ B.compile cl x

exec
  :: B.PCRE
  -> X.Text
  -> Maybe Bool
exec r x = unsafePerformIO $ B.exec r x


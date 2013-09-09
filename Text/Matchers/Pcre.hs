{-# LANGUAGE Trustworthy #-}
module Text.Matchers.Pcre
  ( B.Caseless
  , B.Regex
  , B.reCaseless
  , B.rePattern
  , compile
  , exec
  ) where

import qualified Text.Matchers.Pcre.Base as B
import qualified Data.Text as X
import System.IO.Unsafe (unsafePerformIO)

compile
  :: B.Caseless
  -> X.Text
  -> Either String B.Regex
compile cl x = unsafePerformIO $ B.compile cl x

exec
  :: B.Regex
  -> X.Text
  -> Maybe Bool
exec r x = unsafePerformIO $ B.exec r x


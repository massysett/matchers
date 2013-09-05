{-# LANGUAGE Trustworthy #-}
module Text.Matchers.PcrePretty where

import qualified Text.Matchers.PcreBase as B
import qualified Data.Text as X
import System.IO.Unsafe (unsafePerformIO)

type Caseless = Bool

compile :: Caseless -> X.Text -> Either String B.Regexp
compile c x = unsafePerformIO $ B.pcre_compile c x

exec :: B.Regexp -> X.Text -> Maybe Bool
exec r s = unsafePerformIO $ B.pcre_exec r s

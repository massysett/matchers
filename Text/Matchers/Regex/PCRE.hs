module Text.Matchers.Regex.PCRE (pcre) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception, Success))
import qualified Data.ByteString as BS
import qualified Text.Regex.PCRE.Light as PCRE

import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))

pcre :: CaseSensitive
        -> BS.ByteString
        -> Exceptional String (BS.ByteString -> Bool)
pcre c bs = let
  u8 = [PCRE.utf8]
  opts = case c of
    Sensitive -> u8
    Insensitive -> PCRE.caseless:u8 in
  case PCRE.compileM bs opts of
    (Left err) -> Exception err
    (Right rx) -> Success $ \s ->
      case PCRE.match rx s [] of
        (Just _) -> True
        Nothing -> False

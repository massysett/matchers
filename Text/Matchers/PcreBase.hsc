{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, Safe #-}

module Text.Matchers.PcreBase where

import Foreign.C
import Foreign.Ptr
import Data.ByteString
import Data.Text
import Data.Text.Encoding

#include <pcre.h>

caseless :: CInt
caseless = #const PCRE_CASELESS

data PCRE
data PCRE_Extra

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile
    :: CString
    -- ^ Pattern
    -> CInt
    -- ^ Options
    -> Ptr CString
    -- ^ Pointer to error message
    -> Ptr CInt
    -- ^ Pointer to error offset
    -> Ptr CUChar
    -- ^ Pointer to table
    -> IO (Ptr PCRE)

foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec
    :: Ptr PCRE
    -- ^ Regex
    -> Ptr PCRE_Extra
    -- ^ Result of study
    -> CString
    -- ^ Subject
    -> CInt
    -- ^ Length
    -> CInt
    -- ^ Start offset
    -> CInt
    -- ^ Options
    -> Ptr CInt
    -- ^ Output vector
    -> CInt
    -- ^ Output vector size
    -> IO CInt
    -- ^ One more than the highest numbered pair that has been set.

type Caseless = Bool

data Regexp = Regexp (Ptr PCRE)

pcre_compile
  :: Caseless
  -> Text
  -- ^ Pattern
  -> IO (Either String Regexp)
  -- ^ Errors are indicated with a Left with the error message.
pcre_compile cl pat = useAsCString (encodeUtf8 pat) $ \patC -> do
  let cOpt = if cl then caseless else 0
      

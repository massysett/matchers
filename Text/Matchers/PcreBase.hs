{-# LINE 1 "PcreBase.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, Safe #-}
{-# LINE 2 "PcreBase.hsc" #-}

module Text.Matchers.PcreBase where

import Foreign.C
import Foreign.Ptr
import Data.ByteString


{-# LINE 10 "PcreBase.hsc" #-}

caseless :: CInt
caseless = 1
{-# LINE 13 "PcreBase.hsc" #-}

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

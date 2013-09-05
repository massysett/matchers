{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, Safe #-}

module Text.Matchers.PcreBase where

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Data.ByteString
import Data.Text
import Data.Text.Encoding

#include <pcre.h>

caseless :: CInt
caseless = #const PCRE_CASELESS

data PCRE

instance Show PCRE where
  show _ = "PCRE"

data PCRE_Extra

instance Show PCRE_Extra where
  show _ = "PCRE_Extra"

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

data Regexp = Regexp Text Caseless (ForeignPtr PCRE)
  deriving Show

pcre_compile
  :: Caseless
  -> Text
  -- ^ Pattern
  -> IO (Either String Regexp)
  -- ^ Errors are indicated with a Left with the error message.
pcre_compile cl pat
  = useAsCString (encodeUtf8 pat) $ \patC ->
    alloca $ \ptrMsg ->
    alloca $ \ptrOffset -> do
      let cOpt = if cl then caseless else 0
      ptrPcre <- c_pcre_compile patC cOpt ptrMsg ptrOffset nullPtr
      if ptrPcre == nullPtr
        then do
          ptrErr <- peek ptrMsg
          msg <- peekCAString ptrErr
          return . Left $ msg
        else do
          fp <- newForeignPtr finalizerFree ptrPcre
          return . Right $ Regexp pat cl fp
          
  
pcre_exec :: Regexp -> Text -> IO (Maybe Bool)
pcre_exec (Regexp _ _ rePtr) txt
  = useAsCStringLen (encodeUtf8 txt) $ \(ptrSubj, len) ->
    allocaArray 30 $ \array ->
    withForeignPtr rePtr $ \fp -> do
      r <- c_pcre_exec fp nullPtr ptrSubj (fromIntegral len)
                       0 0 array 30
      return $ case () of
        _ | r == (-1) -> Just False
          | r < (-1) -> Nothing
          | otherwise -> Just True
        

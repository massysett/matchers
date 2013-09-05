{-# LINE 1 "PcreBase.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, Safe #-}
{-# LINE 2 "PcreBase.hsc" #-}

module Text.Matchers.PcreBase where

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.ByteString
import Data.Text
import Data.Text.Encoding


{-# LINE 15 "PcreBase.hsc" #-}

caseless :: CInt
caseless = 1
{-# LINE 18 "PcreBase.hsc" #-}

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

foreign import ccall unsafe "pcre.h pcre_free"
  c_pcre_free
    :: Ptr a
    -> IO (Ptr a)

pcre_compile
  :: Caseless
  -> Text
  -- ^ Pattern
  -> IO (Either String Regexp)
  -- ^ Errors are indicated with a Left with the error message.
  -- The Regexp must be freed after use.
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
        else return . Right . Regexp $ ptrPcre
  
      
pcre_free :: Regexp -> IO ()
pcre_free (Regexp ptrPc) = fmap (const ()) $ c_pcre_free ptrPc

pcre_exec :: Regexp -> Text -> IO (Maybe Bool)
pcre_exec (Regexp rePtr) txt
  = useAsCStringLen (encodeUtf8 txt) $ \(ptrSubj, len) ->
    allocaArray 30 $ \array -> do
      r <- c_pcre_exec rePtr nullPtr ptrSubj (fromIntegral len)
                       0 0 array 30
      return $ case () of
        _ | r == (-1) -> Just False
          | r < (-1) -> Nothing
          | otherwise -> Just True
        

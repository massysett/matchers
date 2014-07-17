{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Matchers.Pcre.Base
  ( PCRE
  , compile
  , exec
  ) where

import Foreign.Storable
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Matchers.Types

#include <pcre.h>
#include <stdlib.h>

caseless :: CInt
caseless = #const PCRE_CASELESS

data PCREData

instance Show PCREData where
  show _ = "PCREData"

data PCRE_Extra

instance Show PCRE_Extra where
  show _ = "PCRE_Extra"

foreign import ccall unsafe "stdlib.h &free"
  c_free :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile
    :: CString
    -- ^ Pattern
    -> CInt
    -- ^ Options
    -> Ptr CString
    -- ^ OUT error message
    -> Ptr CInt
    -- ^ OUT Error offset
    -> Ptr CUChar
    -- ^ Pointer to character table. Use NULL for default.
    -> IO (Ptr PCREData)

foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec
    :: Ptr PCREData
    -- ^ Regex

    -> Ptr PCRE_Extra
    -- ^ Result of study.  Just pass NULL if you did not study the
    -- pattern.

    -> CString
    -- ^ Subject
    -> CInt
    -- ^ Length of subject string.  (Apparently it does not have to be
    -- null terminated?)
    -> CInt
    -- ^ Start at this offset in the subject string.
    -> CInt
    -- ^ Options
    -> Ptr CInt
    -- ^ OUT Output vector.  Information about matching substrings is
    -- stored in this array.
    -> CInt
    -- ^ Output vector size
    -> IO CInt
    -- ^ One more than the highest numbered pair that has been set.

pcre_compile
  :: CaseSensitive
  -> Text
  -- ^ Pattern
  -> IO (Either String (Ptr PCREData))
  -- ^ Errors are indicated with a Left with the error message.
pcre_compile cl pat
  = useAsCString (encodeUtf8 pat) $ \patC ->
    alloca $ \ptrMsg ->
    alloca $ \ptrOffset -> do
      let cOpt = if cl == Insensitive then caseless else 0
      ptrPcre <- c_pcre_compile patC cOpt ptrMsg ptrOffset nullPtr
      if ptrPcre == nullPtr
        then do
          ptrErr <- peek ptrMsg
          msg <- peekCAString ptrErr
          return . Left $ msg
        else return . Right $ ptrPcre
          
  
pcre_exec :: Ptr PCREData -> Text -> IO (Maybe Bool)
pcre_exec ptr txt
  = useAsCStringLen (encodeUtf8 txt) $ \(ptrSubj, len) ->
    allocaArray 30 $ \array -> do
      r <- c_pcre_exec ptr nullPtr ptrSubj (fromIntegral len)
                       0 0 array 30
      return $ case () of
        _ | r == (-1) -> Just False
          | r < (-1) -> Nothing
          | otherwise -> Just True
        
newtype PCRE = PCRE (ForeignPtr PCREData)
  deriving Show

compile :: CaseSensitive -> Text -> IO (Either String PCRE)
compile cl pat = do
  ei <- pcre_compile cl pat
  case ei of
    Left e -> return . Left $ e
    Right ptr -> do
      fp <- newForeignPtr c_free ptr
      return . Right $ PCRE fp

exec :: PCRE -> Text -> IO (Maybe Bool)
exec (PCRE fp) s = withForeignPtr fp $ \p -> pcre_exec p s

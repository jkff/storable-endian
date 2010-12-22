{-# LANGUAGE MagicHash, TemplateHaskell, CPP #-}
module Data.Storable.Endian where

import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

import Data.Word
import Data.Bits

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
import GHC.Int
#endif

deriveEndian :: String -> Int -> Int -> Q [Dec]
deriveEndian baseName bytes bits = mapM deriveEndian' ["le", "be"]
  where
    deriveEndian' end = do
      let typeName = baseName ++ show bits ++ map toUpper end
      let getter   = reify $ "getWord" ++ show bits ++ end
      let putter   = reify $ "putWord" ++ show bits ++ end

      [d| 
               instance Storable $( typeName ) where
                 sizeOf _ = $(lift bytes)
                 alignment _ = $(lift bytes)
                 peek p = ($(typeName) . unsafeCoerce) `fmap` $(getter) (castPtr p)
                 poke p ($(typeName) x) = $(putter) (unsafeCoerce x) (castPtr p)
        |]

$(deriveEndian 'Int 2 16)
$(deriveEndian 'Int 4 32)
$(deriveEndian 'Int 8 64)

$(deriveEndian 'Word 2 16)
$(deriveEndian 'Word 4 32)
$(deriveEndian 'Word 8 64)

$(deriveEndian 'Float 4 32)
$(deriveEndian 'Double 8 64)

-- The code below has been adapted from the 'binary' library
-- http://hackage.haskell.org/package/binary

------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> Ptr Word8 -> IO ()
putWord16be w p = do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> Ptr Word8 -> IO ()
putWord16le w p = do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE putWord16le #-}

-- putWord16le w16 = writeN 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Ptr Word8 -> IO ()
putWord32be w p = do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> Ptr Word8 -> IO ()
putWord32le w p = do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE putWord32le #-}

-- on a little endian machine:
-- putWord32le w32 = writeN 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Ptr Word8 -> IO ()
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
putWord64be w p = do
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
putWord64be w p = do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> Ptr Word8 -> IO ()

#if WORD_SIZE_IN_BITS < 64
putWord64le w p = do
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    poke (p)             (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
putWord64le w p = do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
{-# INLINE putWord64le #-}

unsafePeekOff :: Ptr Word8 -> Int -> Word8
unsafePeekOff p off = unsafePerformIO (p `peekElemOff` off)

-- on a little endian machine:
-- putWord64le w64 = writeN 8 (\p -> poke (castPtr p) w64)
-- | Read a Word16 in big endian format
getWord16be :: Ptr Word8 -> IO Word16
getWord16be p = do
    return $! (fromIntegral (p `unsafePeekOff` 0) `shiftl_w16` 8) .|.
              (fromIntegral (p `unsafePeekOff` 1))
{-# INLINE getWord16be #-}

-- | Read a Word16 in little endian format
getWord16le :: Ptr Word8 -> IO Word16
getWord16le p = do
    return $! (fromIntegral (p `unsafePeekOff` 1) `shiftl_w16` 8) .|.
              (fromIntegral (p `unsafePeekOff` 0) )
{-# INLINE getWord16le #-}

-- | Read a Word32 in big endian format
getWord32be :: Ptr Word8 -> IO Word32
getWord32be p = do
    return $! (fromIntegral (p `unsafePeekOff` 0) `shiftl_w32` 24) .|.
              (fromIntegral (p `unsafePeekOff` 1) `shiftl_w32` 16) .|.
              (fromIntegral (p `unsafePeekOff` 2) `shiftl_w32`  8) .|.
              (fromIntegral (p `unsafePeekOff` 3) )
{-# INLINE getWord32be #-}

-- | Read a Word32 in little endian format
getWord32le :: Ptr Word8 -> IO Word32
getWord32le p = do
    return $! (fromIntegral (p `unsafePeekOff` 3) `shiftl_w32` 24) .|.
              (fromIntegral (p `unsafePeekOff` 2) `shiftl_w32` 16) .|.
              (fromIntegral (p `unsafePeekOff` 1) `shiftl_w32`  8) .|.
              (fromIntegral (p `unsafePeekOff` 0) )
{-# INLINE getWord32le #-}

-- | Read a Word64 in big endian format
getWord64be :: Ptr Word8 -> IO Word64
getWord64be p = do
    return $! (fromIntegral (p `unsafePeekOff` 0) `shiftl_w64` 56) .|.
              (fromIntegral (p `unsafePeekOff` 1) `shiftl_w64` 48) .|.
              (fromIntegral (p `unsafePeekOff` 2) `shiftl_w64` 40) .|.
              (fromIntegral (p `unsafePeekOff` 3) `shiftl_w64` 32) .|.
              (fromIntegral (p `unsafePeekOff` 4) `shiftl_w64` 24) .|.
              (fromIntegral (p `unsafePeekOff` 5) `shiftl_w64` 16) .|.
              (fromIntegral (p `unsafePeekOff` 6) `shiftl_w64`  8) .|.
              (fromIntegral (p `unsafePeekOff` 7) )
{-# INLINE getWord64be #-}

-- | Read a Word64 in little endian format
getWord64le :: Ptr Word8 -> IO Word64
getWord64le p = do
    return $! (fromIntegral (p `unsafePeekOff` 7) `shiftl_w64` 56) .|.
              (fromIntegral (p `unsafePeekOff` 6) `shiftl_w64` 48) .|.
              (fromIntegral (p `unsafePeekOff` 5) `shiftl_w64` 40) .|.
              (fromIntegral (p `unsafePeekOff` 4) `shiftl_w64` 32) .|.
              (fromIntegral (p `unsafePeekOff` 3) `shiftl_w64` 24) .|.
              (fromIntegral (p `unsafePeekOff` 2) `shiftl_w64` 16) .|.
              (fromIntegral (p `unsafePeekOff` 1) `shiftl_w64`  8) .|.
              (fromIntegral (p `unsafePeekOff` 0) )
{-# INLINE getWord64le #-}

------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Parts (
	ptr, gen, new, with, stride,
	CairoFormatT,
	pattern CairoFormatArgb32, pattern CairoFormatRgb24,
	pattern CairoFormatA8, pattern CairoFormatA1,
	pattern CairoFormatRgb16_565, pattern CairoFormatRgb30 ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal (mallocBytes, free)
import Foreign.Storable (Storable, sizeOf, alignment, poke)
import Foreign.C.Types (CInt(..))
import Control.Monad.Primitive (
	PrimMonad(..), PrimBase, unsafeIOToPrim, unsafePrimToIO )
import Data.Foldable (for_)
import Data.Int (Int32)

#include <cairo.h>

type CairoFormatT = #type cairo_format_t

---------------------------------------------------------------------------

-- *

---------------------------------------------------------------------------
-- COMMON
---------------------------------------------------------------------------

ptr :: forall a . Storable a => CInt -> CInt -> CInt -> Ptr a -> CInt -> CInt -> Maybe (Ptr a)
ptr w h st p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` (fromIntegral y * fromIntegral st + fromIntegral x * u)
	| otherwise = Nothing
	where
	sz = sizeOf (undefined :: a)
	al = alignment (undefined :: a)
	u = ((sz - 1) `div` al + 1) * al

gen :: (PrimBase m, Storable a) => CInt -> CInt -> CInt -> (CInt -> CInt -> m a) -> m (ForeignPtr a)
gen w h s f = unsafeIOToPrim do
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptr w h s d x y
	newForeignPtr d $ free d

new :: PrimMonad m => CInt -> CInt -> m (ForeignPtr a)
new s h = unsafeIOToPrim $ mallocBytes (fromIntegral $ s * h) >>= \d -> newForeignPtr d $ free d

with :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
with = withForeignPtr

foreign import ccall "cairo_format_stride_for_width"
	c_cairo_format_stride_for_width :: #{type cairo_format_t} -> CInt -> IO CInt

stride :: PrimMonad m => #{type cairo_format_t} -> CInt -> m CInt
stride f w = unsafeIOToPrim $ c_cairo_format_stride_for_width f w

pattern CairoFormatArgb32 :: #{type cairo_format_t}
pattern CairoFormatArgb32 <- #{const CAIRO_FORMAT_ARGB32} where
	CairoFormatArgb32 = #{const CAIRO_FORMAT_ARGB32}

pattern CairoFormatRgb24 :: #{type cairo_format_t}
pattern CairoFormatRgb24 <- #{const CAIRO_FORMAT_RGB24} where
	CairoFormatRgb24 = #{const CAIRO_FORMAT_RGB24}

pattern CairoFormatA8 :: #{type cairo_format_t}
pattern CairoFormatA8 <- #{const CAIRO_FORMAT_A8} where
	CairoFormatA8 = #{const CAIRO_FORMAT_A8}

pattern CairoFormatA1 :: #{type cairo_format_t}
pattern CairoFormatA1 <- #{const CAIRO_FORMAT_A1} where
	CairoFormatA1 = #{const CAIRO_FORMAT_A1}

pattern CairoFormatRgb16_565 :: #{type cairo_format_t}
pattern CairoFormatRgb16_565 <- #{const CAIRO_FORMAT_RGB16_565} where
	CairoFormatRgb16_565 = #{const CAIRO_FORMAT_RGB16_565}

pattern CairoFormatRgb30 :: #{type cairo_format_t}
pattern CairoFormatRgb30 <- #{const CAIRO_FORMAT_RGB30} where
	CairoFormatRgb30 = #{const CAIRO_FORMAT_RGB30}

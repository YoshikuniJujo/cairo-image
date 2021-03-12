{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Internal (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage(..), CairoImageMut(..), cairoImageFreeze, cairoImageThaw,
	-- * Image Format
	-- ** ARGB 32
	PixelArgb32(..),
	pattern PixelArgb32Premultiplied, pixelArgb32Premultiplied,
	pattern PixelArgb32Straight,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	-- ** RGB 24
	PixelRgb24(..), pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24,
	pattern CairoImageMutRgb24, Rgb24Mut,
	-- ** A 8
	PixelA8(..),
	pattern CairoImageA8, A8,
	pattern CairoImageMutA8, A8Mut,
	-- ** A 1
	PixelA1(..), Bit(..), bit,
	pattern CairoImageA1, A1,
	pattern CairoImageMutA1, A1Mut,
	-- ** RGB 16 565
	PixelRgb16_565(..), pattern PixelRgb16_565,
	pattern CairoImageRgb16_565, Rgb16_565,
	pattern CairoImageMutRgb16_565, Rgb16_565Mut,
	-- ** RGB 30
	PixelRgb30(..), pattern PixelRgb30,
	pattern CairoImageRgb30, Rgb30,
	pattern CairoImageMutRgb30, Rgb30Mut
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Bits (
	Bits, (.&.), (.|.), testBit, clearBit, setBit, shift, shiftL, shiftR)
import Data.Word
import Data.Int
import System.IO.Unsafe
import System.TargetEndian

#include <cairo.h>

foreign import ccall "cairo_format_stride_for_width"
	c_cairo_format_stride_for_width ::
	#{type cairo_format_t} -> #{type int} -> IO #{type int}

data CairoImage = CairoImage {
	cairoImageFormat :: #{type cairo_format_t},
	cairoImageWidth :: #{type int},
	cairoImageHeight :: #{type int},
	cairoImageStride :: #{type int},
	cairoImageData :: ForeignPtr #{type unsigned char} }
	deriving Show

data CairoImageMut s = CairoImageMut {
	cairoImageMutFormat :: #{type cairo_format_t},
	cairoImageMutWidth :: #{type int},
	cairoImageMutHeight :: #{type int},
	cairoImageMutStride :: #{type int},
	cairoImageMutData :: ForeignPtr #{type unsigned char} }
	deriving Show

cairoImageDataCopy :: #{type int} -> #{type int} ->
	ForeignPtr #{type unsigned char} -> IO (ForeignPtr #{type unsigned char})
cairoImageDataCopy str h fdt = withForeignPtr fdt \dt -> do
	dt' <- mallocBytes . fromIntegral $ str * h
	copyBytes dt' dt . fromIntegral $ str * h
	newForeignPtr dt' (free dt')

cairoImageFreeze :: PrimMonad m =>
	CairoImageMut (PrimState m) -> m CairoImage
cairoImageFreeze cim = unsafeIOToPrim
	$ CairoImage fmt w h str <$> cairoImageDataCopy str h dt
	where
	fmt = cairoImageMutFormat cim
	w = cairoImageMutWidth cim
	h = cairoImageMutHeight cim
	str = cairoImageMutStride cim
	dt = cairoImageMutData cim

cairoImageThaw :: PrimMonad m =>
	CairoImage -> m (CairoImageMut (PrimState m))
cairoImageThaw ci = unsafeIOToPrim
	$ CairoImageMut fmt w h str <$> cairoImageDataCopy str h dt
	where
	fmt = cairoImageFormat ci
	w = cairoImageWidth ci
	h = cairoImageHeight ci
	str = cairoImageStride ci
	dt = cairoImageData ci

instance Eq CairoImage where
	ci1 == ci2 = and [
		fmt1 == fmt2, w1 == w2, h1 == h2, str1 == str2,
		unsafePerformIO
			$ withForeignPtr fd1 \d1 -> withForeignPtr fd2 \d2 ->
				compareBytes d1 d2 (str1 * h1) >>=
					\case EQ -> pure True; _ -> pure False ]
		where
		[fmt1, fmt2] = cairoImageFormat <$> [ci1, ci2]
		[w1, w2] = cairoImageWidth <$> [ci1, ci2]
		[h1, h2] = cairoImageHeight <$> [ci1, ci2]
		[str1, str2] = cairoImageStride <$> [ci1, ci2]
		[fd1, fd2] = cairoImageData <$> [ci1, ci2]

compareBytes :: (Ord n, Num n) => Ptr a -> Ptr a -> n -> IO Ordering
compareBytes _ _ n | n < 1 = pure EQ
compareBytes p1 p2 _ | p1 == p2 = pure EQ
compareBytes p1 p2 n = compare <$> peek p1b <*> peek p2b >>=
	\case EQ -> compareBytes p1 p2 (n - 1); o -> pure o
	where [p1b, p2b] = castPtr <$> [p1, p2] :: [Ptr Word8]

pattern CairoImageArgb32 :: Argb32 -> CairoImage
pattern CairoImageArgb32 a <- (cairoImageToArgb32 -> Just a)
	where CairoImageArgb32 (Argb32 w h s d) =
		CairoImage #{const CAIRO_FORMAT_ARGB32} w h s $ castForeignPtr d

cairoImageToArgb32 :: CairoImage -> Maybe Argb32
cairoImageToArgb32 = \case
	CairoImage #{const CAIRO_FORMAT_ARGB32} w h s d ->
		Just . Argb32 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageRgb24 :: Rgb24 -> CairoImage
pattern CairoImageRgb24 r <- (cairoImageToRgb24 -> Just r)
	where CairoImageRgb24 (Rgb24 w h s d) =
		CairoImage #{const CAIRO_FORMAT_RGB24} w h s $ castForeignPtr d

cairoImageToRgb24 :: CairoImage -> Maybe Rgb24
cairoImageToRgb24 = \case
	CairoImage #{const CAIRO_FORMAT_RGB24} w h s d ->
		Just . Rgb24 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageRgb16_565 :: Rgb16_565 -> CairoImage
pattern CairoImageRgb16_565 r <- (cairoImageToRgb16_565 -> Just r)
	where CairoImageRgb16_565 (Rgb16_565 w h s d) =
		CairoImage
			#{const CAIRO_FORMAT_RGB16_565}
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d

cairoImageToRgb16_565 :: CairoImage -> Maybe Rgb16_565
cairoImageToRgb16_565 = \case
	CairoImage #{const CAIRO_FORMAT_RGB16_565} w h s d ->
		Just . Rgb16_565
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d
	_ -> Nothing

pattern CairoImageRgb30 :: Rgb30 -> CairoImage
pattern CairoImageRgb30 r <- (cairoImageToRgb30 -> Just r)
	where CairoImageRgb30 (Rgb30 w h s d) =
		CairoImage
			#{const CAIRO_FORMAT_RGB30}
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d

cairoImageToRgb30 :: CairoImage -> Maybe Rgb30
cairoImageToRgb30 = \case
	CairoImage #{const CAIRO_FORMAT_RGB30} w h s d ->
		Just . Rgb30
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d
	_ -> Nothing

pattern CairoImageA8 :: A8 -> CairoImage
pattern CairoImageA8 a <- (cairoImageToA8 -> Just a)
	where CairoImageA8 (A8 w h s d) =
		CairoImage #{const CAIRO_FORMAT_A8} w h s $ castForeignPtr d

cairoImageToA8 :: CairoImage -> Maybe A8
cairoImageToA8 = \case
	CairoImage #{const CAIRO_FORMAT_A8} w h s d ->
		Just . A8 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageA1 :: A1 -> CairoImage
pattern CairoImageA1 a <- (cairoImageToA1 -> Just a)
	where CairoImageA1 (A1 w h s d) =
		CairoImage #{const CAIRO_FORMAT_A1} w h s $ castForeignPtr d

cairoImageToA1 :: CairoImage -> Maybe A1
cairoImageToA1 = \case
	CairoImage #{const CAIRO_FORMAT_A1} w h s d ->
		Just . A1 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutRgb24 :: Rgb24Mut s -> CairoImageMut s
pattern CairoImageMutRgb24 r <- (cairoImageMutToRgb24 -> Just r)
	where CairoImageMutRgb24 (Rgb24Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_RGB24} w h s $ castForeignPtr d

cairoImageMutToRgb24 :: CairoImageMut s -> Maybe (Rgb24Mut s)
cairoImageMutToRgb24 = \case
	CairoImageMut #{const CAIRO_FORMAT_RGB24} w h s d ->
		Just . Rgb24Mut w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutRgb16_565 :: Rgb16_565Mut s -> CairoImageMut s
pattern CairoImageMutRgb16_565 r <- (cairoImageMutToRgb16_565 -> Just r)
	where CairoImageMutRgb16_565 (Rgb16_565Mut w h s d) =
		CairoImageMut
			#{const CAIRO_FORMAT_RGB16_565}
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d

cairoImageMutToRgb16_565 :: CairoImageMut s -> Maybe (Rgb16_565Mut s)
cairoImageMutToRgb16_565 = \case
	CairoImageMut #{const CAIRO_FORMAT_RGB16_565} w h s d ->
		Just . Rgb16_565Mut
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutRgb30 :: Rgb30Mut s -> CairoImageMut s
pattern CairoImageMutRgb30 r <- (cairoImageMutToRgb30 -> Just r)
	where CairoImageMutRgb30 (Rgb30Mut w h s d) =
		CairoImageMut
			#{const CAIRO_FORMAT_RGB30}
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d

cairoImageMutToRgb30 :: CairoImageMut s -> Maybe (Rgb30Mut s)
cairoImageMutToRgb30 = \case
	CairoImageMut #{const CAIRO_FORMAT_RGB30} w h s d ->
		Just . Rgb30Mut
			(fromIntegral w) (fromIntegral h) (fromIntegral s) $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutA8 :: A8Mut s -> CairoImageMut s
pattern CairoImageMutA8 a <- (cairoImageMutToA8 -> Just a)
	where CairoImageMutA8 (A8Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_A8} w h s $ castForeignPtr d

cairoImageMutToA8 :: CairoImageMut s -> Maybe (A8Mut s)
cairoImageMutToA8 = \case
	CairoImageMut #{const CAIRO_FORMAT_A8} w h s d ->
		Just . A8Mut w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutA1 :: A1Mut s -> CairoImageMut s
pattern CairoImageMutA1 a <- (cairoImageMutToA1 -> Just a)
	where CairoImageMutA1 (A1Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_A1} w h s $ castForeignPtr d

cairoImageMutToA1 :: CairoImageMut s -> Maybe (A1Mut s)
cairoImageMutToA1 = \case
	CairoImageMut #{const CAIRO_FORMAT_A1} w h s d ->
		Just . A1Mut w h s $ castForeignPtr d
	_ -> Nothing

data Argb32 = Argb32 {
	argb32Width :: #{type int},
	argb32Height :: #{type int},
	argb32Stride :: #{type int},
	argb32Data :: ForeignPtr PixelArgb32 }
	deriving Show

pattern CairoImageMutArgb32 :: Argb32Mut s -> CairoImageMut s
pattern CairoImageMutArgb32 a <- (cairoImageMutToArgb32 -> Just a)
	where CairoImageMutArgb32 (Argb32Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_ARGB32} w h s $ castForeignPtr d

cairoImageMutToArgb32 :: CairoImageMut s -> Maybe (Argb32Mut s)
cairoImageMutToArgb32 = \case
	CairoImageMut #{const CAIRO_FORMAT_ARGB32} w h s d ->
		Just . Argb32Mut w h s $ castForeignPtr d
	_ -> Nothing

data Argb32Mut s = Argb32Mut {
	argb32MutWidth :: #{type int},
	argb32MutHeight :: #{type int},
	argb32MutStride :: #{type int},
	argb32MutData :: ForeignPtr PixelArgb32 }
	deriving Show

newtype PixelArgb32 = PixelArgb32Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelArgb32Premultiplied #-}

pattern PixelArgb32Premultiplied :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pattern PixelArgb32Premultiplied a r g b <- (pixelArgb32ToArgb -> (a, r, g, b))

pixelArgb32Premultiplied :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe PixelArgb32
pixelArgb32Premultiplied a r g b
	| r <= a, g <= a, b <= a = Just $ pixelArgb32FromArgb a r g b
	| otherwise = Nothing

pixelArgb32FromArgb :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pixelArgb32FromArgb
	(fromIntegral -> a) (fromIntegral -> r)
	(fromIntegral -> g) (fromIntegral -> b) = PixelArgb32Word32
	$ a `shiftL` 24 .|. r `shiftL` 16 .|. g `shift` 8 .|. b

pixelArgb32ToArgb :: PixelArgb32 -> (Word8, Word8, Word8, Word8)
pixelArgb32ToArgb (PixelArgb32Word32 w) = (
	fromIntegral $ w `shiftR` 24, fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

{-# COMPLETE PixelArgb32Straight #-}

pattern PixelArgb32Straight :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pattern PixelArgb32Straight a r g b <- (pixelArgb32ToArgbStraight -> (a, r, g, b))
	where PixelArgb32Straight = pixelArgb32FromArgbStraight

pixelArgb32FromArgbStraight :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pixelArgb32FromArgbStraight a r g b = pixelArgb32FromArgb
	a (r `unit` (a, 0xff)) (g `unit` (a, 0xff)) (b `unit` (a, 0xff))

pixelArgb32ToArgbStraight :: PixelArgb32 -> (Word8, Word8, Word8, Word8)
pixelArgb32ToArgbStraight p = (a, r `unit` (0xff, a), g `unit` (0xff, a), b `unit` (0xff, a))
	where (a, r, g, b) = pixelArgb32ToArgb p

class Image i where
	type Pixel i
	imageSize :: i -> (CInt, CInt)
	pixelAt :: i -> CInt -> CInt -> Maybe (Pixel i)
	generateImage :: CInt -> CInt -> (CInt -> CInt -> Pixel i) -> i
	generateImagePrimM :: PrimBase m => CInt -> CInt -> (CInt -> CInt -> m (Pixel i)) -> m i

	generateImage w h f = runST $ generateImagePrimM w h \x y -> pure $ f x y

class ImageMut im where
	type PixelMut im
	imageMutSize :: im s -> (#{type int}, #{type int})
	getPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> m (Maybe (PixelMut im))
	newImageMut :: PrimMonad m =>
		#{type int} -> #{type int} -> m (im (PrimState m))
	putPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> PixelMut im -> m ()

instance Image Argb32 where
	type Pixel Argb32 = PixelArgb32
	imageSize (Argb32 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f = generateArgb32PrimM
		(fromIntegral w) (fromIntegral h) \x y -> f (fromIntegral x) (fromIntegral y)
	pixelAt (Argb32 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p
			(fromIntegral x) (fromIntegral y)

instance Image Rgb24 where
	type Pixel Rgb24 = PixelRgb24
	imageSize (Rgb24 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f = generateRgb24PrimM
		(fromIntegral w) (fromIntegral h) \x y -> f (fromIntegral x) (fromIntegral y)
	pixelAt (Rgb24 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrRgb24 w h s p
			(fromIntegral x) (fromIntegral y)

instance Image Rgb16_565 where
	type Pixel Rgb16_565 = PixelRgb16_565
	imageSize (Rgb16_565 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f = generateRgb16_565PrimM
		(fromIntegral w) (fromIntegral h)
		\x y -> f (fromIntegral x) (fromIntegral y)
	pixelAt (Rgb16_565 w h s d) (fromIntegral -> x) (fromIntegral -> y) = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrRgb16_565 w h s p x y

instance Image Rgb30 where
	type Pixel Rgb30 = PixelRgb30
	imageSize (Rgb30 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f = generateRgb30PrimM
		(fromIntegral w) (fromIntegral h)
		\x y -> f (fromIntegral x) (fromIntegral y)
	pixelAt (Rgb30 w h s d) (fromIntegral -> x) (fromIntegral -> y) = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrRgb30 w h s p x y

instance Image A8 where
	type Pixel A8 = PixelA8
	imageSize (A8 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f = generateA8PrimM
		(fromIntegral w) (fromIntegral h) \x y -> f (fromIntegral x) (fromIntegral y)
	pixelAt (A8 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrA8 w h s p
			(fromIntegral x) (fromIntegral y)

generateArgb32PrimM :: PrimBase	m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelArgb32) -> m Argb32
generateArgb32PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrArgb32 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ Argb32 w h s fd

generateRgb24PrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelRgb24) -> m Rgb24
generateRgb24PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB24} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrRgb24 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ Rgb24 w h s fd

generateRgb16_565PrimM :: PrimBase m => CInt -> CInt -> (CInt -> CInt -> m PixelRgb16_565) -> m Rgb16_565
generateRgb16_565PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB16_565} $ fromIntegral w
	d <- mallocBytes . fromIntegral $ fromIntegral s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrRgb16_565 w h (fromIntegral s) d x y
	fd <- newForeignPtr d $ free d
	pure $ Rgb16_565 w h (fromIntegral s) fd

generateRgb30PrimM :: PrimBase m => CInt -> CInt -> (CInt -> CInt -> m PixelRgb30) -> m Rgb30
generateRgb30PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB30} $ fromIntegral w
	d <- mallocBytes . fromIntegral $ fromIntegral s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrRgb30 w h (fromIntegral s) d x y
	fd <- newForeignPtr d $ free d
	pure $ Rgb30 w h (fromIntegral s) fd

generateA8PrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelA8) -> m A8
generateA8PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_A8} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrA8 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ A8 w h s fd

generateA1PrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelA1) -> m A1
generateA1PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_A8} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (\(pt, i) -> pokeA1 pt i p) $ ptrA1 w h s d x y
	let	d' = castPtr d
	fd <- newForeignPtr d' $ free d'
	pure $ A1 w h s fd

instance ImageMut Argb32Mut where
	type PixelMut Argb32Mut = PixelArgb32
	imageMutSize (Argb32Mut w h _ _) = (w, h)
	newImageMut = newArgb32Mut
	getPixel (Argb32Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y
	putPixel (Argb32Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrArgb32 w h s p x y

instance ImageMut Rgb24Mut where
	type PixelMut Rgb24Mut = PixelRgb24
	imageMutSize (Rgb24Mut w h _ _) = (w, h)
	newImageMut = newRgb24Mut
	getPixel (Rgb24Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrRgb24 w h s p x y
	putPixel (Rgb24Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrRgb24 w h s p x y

instance ImageMut Rgb16_565Mut where
	type PixelMut Rgb16_565Mut = PixelRgb16_565
	imageMutSize (Rgb16_565Mut w h _ _) = (fromIntegral w, fromIntegral h)
	newImageMut w h = newRgb16_565Mut (fromIntegral w) (fromIntegral h)
	getPixel (Rgb16_565Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek)
			$ ptrRgb16_565 w h s p (fromIntegral x) (fromIntegral y)
	putPixel (Rgb16_565Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px)
			$ ptrRgb16_565 w h s p (fromIntegral x) (fromIntegral y)

instance ImageMut Rgb30Mut where
	type PixelMut Rgb30Mut = PixelRgb30
	imageMutSize (Rgb30Mut w h _ _) = (fromIntegral w, fromIntegral h)
	newImageMut w h = newRgb30Mut (fromIntegral w) (fromIntegral h)
	getPixel (Rgb30Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek)
			$ ptrRgb30 w h s p (fromIntegral x) (fromIntegral y)
	putPixel (Rgb30Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px)
			$ ptrRgb30 w h s p (fromIntegral x) (fromIntegral y)

newArgb32Mut :: PrimMonad m => #{type int} -> #{type int} -> m (Argb32Mut (PrimState m))
newArgb32Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ Argb32Mut w h s fd

newRgb24Mut :: PrimMonad m => #{type int} -> #{type int} -> m (Rgb24Mut (PrimState m))
newRgb24Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB24} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ Rgb24Mut w h s fd

newRgb16_565Mut :: PrimMonad m => CInt -> CInt -> m (Rgb16_565Mut (PrimState m))
newRgb16_565Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB16_565} $ fromIntegral w
	d <- mallocBytes . fromIntegral $ fromIntegral s * h
	fd <- newForeignPtr d $ free d
	pure $ Rgb16_565Mut w h (fromIntegral s) fd

newRgb30Mut :: PrimMonad m => CInt -> CInt -> m (Rgb30Mut (PrimState m))
newRgb30Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB30} $ fromIntegral w
	d <- mallocBytes . fromIntegral $ fromIntegral s * h
	fd <- newForeignPtr d $ free d
	pure $ Rgb30Mut w h (fromIntegral s) fd

instance ImageMut A8Mut where
	type PixelMut A8Mut = PixelA8
	imageMutSize (A8Mut w h _ _) = (w, h)
	newImageMut = newA8Mut
	getPixel (A8Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrA8 w h s p x y
	putPixel (A8Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrA8 w h s p x y

newA8Mut :: PrimMonad m => #{type int} -> #{type int} -> m (A8Mut (PrimState m))
newA8Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_A8} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ A8Mut w h s fd

ptrArgb32 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelArgb32 -> #{type int} -> #{type int} -> Maybe (Ptr PixelArgb32)
ptrArgb32 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

ptrRgb24 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelRgb24 -> #{type int} -> #{type int} -> Maybe (Ptr PixelRgb24)
ptrRgb24 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

ptrRgb16_565 :: CInt -> CInt -> CInt ->
	Ptr PixelRgb16_565 -> CInt -> CInt -> Maybe (Ptr PixelRgb16_565)
ptrRgb16_565 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 2)
	| otherwise = Nothing

ptrRgb30 :: CInt -> CInt -> CInt ->
	Ptr PixelRgb30 -> CInt -> CInt -> Maybe (Ptr PixelRgb30)
ptrRgb30 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

ptrA8 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelA8 -> #{type int} -> #{type int} -> Maybe (Ptr PixelA8)
ptrA8 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x)
	| otherwise = Nothing

ptrA1 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelA1 -> #{type int} -> #{type int} -> Maybe (Ptr PixelA1, #{type int})
ptrA1 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just (p `plusPtr` fromIntegral (y * s + x `div` 32 * 4), x `mod` 32)
	| otherwise = Nothing

peekA1 :: Ptr PixelA1 -> #{type int} -> IO PixelA1
peekA1 p i = do
	w32 <- peek (castPtr p) :: IO Word32
	pure . PixelA1 . toBit $ w32 `testBit` (fromIntegral $(endian [e| i |] [e| 31 - i |]))

pokeA1 :: Ptr PixelA1 -> #{type int} -> PixelA1 -> IO ()
pokeA1 p i (PixelA1 b) = do
	w32 <- peek (castPtr p) :: IO Word32
	poke (castPtr p) (put w32 (fromIntegral $(endian [e| i |] [e| 31 - i |])) b)

newtype PixelRgb24 = PixelRgb24Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelRgb24 #-}

pattern PixelRgb24 :: Word8 -> Word8 -> Word8 -> PixelRgb24
pattern PixelRgb24 r g b <- (pixelRgb24ToRgb -> (r, g, b))
	where PixelRgb24 = pixelRgb24FromRgb

pixelRgb24FromRgb :: Word8 -> Word8 -> Word8 -> PixelRgb24
pixelRgb24FromRgb (fromIntegral -> r)
	(fromIntegral -> g) (fromIntegral -> b) =
	PixelRgb24Word32 $ r `shiftL` 16 .|. g `shift` 8 .|. b

pixelRgb24ToRgb :: PixelRgb24 -> (Word8, Word8, Word8)
pixelRgb24ToRgb (PixelRgb24Word32 w) = (
	fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

data Rgb24 = Rgb24 {
	rgb24Width :: #{type int}, rgb24Height :: #{type int},
	rgb24Stride :: #{type int}, rgb24Data :: ForeignPtr PixelRgb24 }
	deriving Show

data Rgb24Mut s = Rgb24Mut {
	rgb24MutWidth :: #{type int}, rgb24MutHeight :: #{type int},
	rgb24MutStride :: #{type int}, rgb24MutData :: ForeignPtr PixelRgb24 }
	deriving Show

newtype PixelRgb16_565 = PixelRgb16_565Word16 Word16 deriving (Show, Storable)

{-# COMPLETE PixelRgb16_565 #-}

pattern PixelRgb16_565 :: Word8 -> Word8 -> Word8 -> PixelRgb16_565
pattern PixelRgb16_565 r g b <- (pixelRgb16_565ToRgb -> (r, g, b))
	where PixelRgb16_565 = pixelRgb16_565FromRgb

pixelRgb16_565FromRgb :: Word8 -> Word8 -> Word8 -> PixelRgb16_565
pixelRgb16_565FromRgb
	(fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
	PixelRgb16_565Word16 $ r' .|. g' .|. b'
	where
	r' = (r .&. 0xf8) `shiftL` (11 - 3)
	g' = (g .&. 0xfc) `shiftL` (5 - 2)
	b' = (b .&. 0xf8) `shiftR` 3

pixelRgb16_565ToRgb :: PixelRgb16_565 -> (Word8, Word8, Word8)
pixelRgb16_565ToRgb (PixelRgb16_565Word16 rgb) =
	(r .|. r `shiftR` 5, g .|. g `shiftR` 6, b .|. b `shiftR` 5)
	where
	r = fromIntegral $ rgb `shiftR` 11 `shiftL` 3
	g = fromIntegral $ rgb `shiftR` 5 `shiftL` 2
	b = fromIntegral $ rgb `shiftL` 3

data Rgb16_565 = Rgb16_565 {
	rgb16_565Width :: CInt, rgb16_565Height :: CInt,
	rgb16_565Stride :: CInt, rgb16_565Data :: ForeignPtr PixelRgb16_565 }
	deriving Show

data Rgb16_565Mut s = Rgb16_565Mut {
	rgb16_565MutWidth :: CInt, rgb16_565MutHeight :: CInt,
	rgb16_565MutStride :: CInt, rgb16_565MutData :: ForeignPtr PixelRgb16_565 }
	deriving Show

newtype PixelRgb30 = PixelRgb30Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelRgb30 #-}

pattern PixelRgb30 :: Word16 -> Word16 -> Word16 -> PixelRgb30
pattern PixelRgb30 r g b <- (pixelRgb30ToRgb -> (r, g, b))
	where PixelRgb30 = pixelRgb30FromRgb

pixelRgb30FromRgb :: Word16 -> Word16 -> Word16 -> PixelRgb30
pixelRgb30FromRgb
	(fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
	PixelRgb30Word32 $ r' .|. g' .|. b'
	where
	r' = (r .&. 0xffc0) `shiftL` (20 - 6)
	g' = (g .&. 0xffc0) `shiftL` (10 - 6)
	b' = (b .&. 0xffc0) `shiftR` 6

pixelRgb30ToRgb :: PixelRgb30 -> (Word16, Word16, Word16)
pixelRgb30ToRgb (PixelRgb30Word32 rgb) =
	(r .|. r `shiftR` 10, g .|. g `shiftR` 10, b .|. b `shiftR` 10)
	where
	r = fromIntegral $ rgb `shiftR` 20 `shiftL` 6
	g = fromIntegral $ rgb `shiftR` 10 `shiftL` 6
	b = fromIntegral $ rgb `shiftL` 6

data Rgb30 = Rgb30 {
	rgb30Width :: CInt, rgb30Height :: CInt,
	rgb30Stride :: CInt, rgb30Data :: ForeignPtr PixelRgb30 }
	deriving Show

data Rgb30Mut s = Rgb30Mut {
	rgb30MutWidth :: CInt, rgb30MutHeight :: CInt,
	rgb30MutStride :: CInt, rgb30MutData :: ForeignPtr PixelRgb30 }
	deriving Show

newtype PixelA8 = PixelA8 Word8 deriving (Show, Storable)

data A8 = A8 {
	a8Width :: #{type int}, a8Height :: #{type int},
	a8Stride :: #{type int}, a8Data :: ForeignPtr PixelA8 }
	deriving Show

data A8Mut s = A8Mut {
	a8MutWidth :: #{type int}, a8MutHeight :: #{type int},
	a8MutStride :: #{type int}, a8MutData :: ForeignPtr PixelA8 }
	deriving Show

data Bit = O | I deriving (Show, Enum)

bit :: a -> a -> Bit -> a
bit x _ O = x
bit _ y I = y

toBit :: Bool -> Bit
toBit = \case False -> O; True -> I

put :: Bits a => a -> Int -> Bit -> a
put n i O = n `clearBit` i
put n i I = n `setBit` i

newtype PixelA1 = PixelA1 Bit deriving Show -- (Show, Storable)

data A1 = A1 {
	a1Width :: #{type int}, a1Height :: #{type int},
	a1Stride :: #{type int}, a1Data :: ForeignPtr Word32 }
	deriving Show

data A1Mut s = A1Mut {
	a1MutWidth :: #{type int}, a1MutHeight :: #{type int},
	a1MutStride :: #{type int}, a1MutData :: ForeignPtr PixelA1 }
	deriving Show

instance Image A1 where
	type Pixel A1 = PixelA1
	imageSize (A1 w h _ _) = (fromIntegral w, fromIntegral h)
	generateImagePrimM w h f =
		generateA1PrimM (fromIntegral w) (fromIntegral h) (\x y -> f (fromIntegral x) (fromIntegral y))
	pixelAt (A1 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . uncurry peekA1) $ ptrA1 w h s (castPtr p)
			(fromIntegral x) (fromIntegral y)

instance ImageMut A1Mut where
	type PixelMut A1Mut = PixelA1
	imageMutSize (A1Mut w h _ _) = (w, h)
	newImageMut = newA1Mut
	getPixel (A1Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . uncurry peekA1) $ ptrA1 w h s p x y
	putPixel (A1Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (\(pt, i) -> pokeA1 pt i px) $ ptrA1 w h s p x y

newA1Mut :: PrimMonad m => #{type int} -> #{type int} -> m (A1Mut (PrimState m))
newA1Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_A1} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ A1Mut w h s fd

unit :: Word8 -> (Word8, Word8) -> Word8
n `unit` (m, d) = fromIntegral
	(fromIntegral n * fromIntegral m `div'` fromIntegral d :: Word16)

infixl 7 `div'`

div' :: Integral n => n -> n -> n
_ `div'` 0 = 0
n `div'` m = n `div` m

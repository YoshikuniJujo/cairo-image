{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage, CairoImageMut, cairoImageFreeze, cairoImageThaw,
	-- * Image Format
	-- ** ARGB32
	PixelArgb32(..),
	pattern PixelArgb32Premultiplied, pixelArgb32Premultiplied,
	pattern PixelArgb32Straight,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	-- ** RGB24
	PixelRgb24(..), pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24,
	pattern CairoImageMutRgb24, Rgb24Mut
	) where

import Data.CairoImage.Internal

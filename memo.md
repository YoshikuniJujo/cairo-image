memo
====

repair
------

* [x] use CInt instead of #{type int}
	+ [x] class Image
		- [x] imageSize
		- [x] pixelAt
		- [x] generateImage
		- [x] generateImagePrimM
	+ [x] class ImageMut
	+ [x] CairoImage
	+ [x] CairoImageMut
	+ [x] c\_cairo\_format\_stride\_for\_width
	+ [x] format
		- [x] ARGB 32
		- [x] RGB 24
		- [x] A 8
		- [x] A 1
		- [x] RGB 16 565
		- [x] RGB 30
	+ [x] remove fromIntegral
* [x] repair `pattern PixelArgb32`
	+ [x] function `unit`
	+ [x] make `pattern PixelArgb32Straight`
		- [x] function `pixelArgb32FromArgbStrait`
		- [x] function `pixelArgb32ToArgbStrait`
		- [x] `pattern PixelArgb32Straight`
	+ [x] make `pattern PixelArgb32Premultiplied`
	+ [x] make function `pixelArgb32Premultiplied`
		- `Word8 -> Word8 -> Word8 -> Word8 -> Maybe PixelArgb32`
	+ [x] remove `pattern PixelArgb32`
	+ [x] repair JuicyCairo
	+ [x] repair try-cairo

refactoring
-----------

* [x] refactor document
	+ [x] System.TargetEndian
	+ [x] Data.CairoImage.Internal
		- [x] structure
		- [x] Class Image and ImageMut
			* [x] class Image
			* [x] class ImageMut
		- [x] Type CairoImage and CairoImageMut
			* [x] data CairoImage
			* [x] data CairoImageMut
			* [x] function cairoImageFreeze
			* [x] function cairoImageThaw
		- [x] Image Format
			* [x] ARGB 32
			* [x] RGB 24
			* [x] A 8
			* [x] A 1
			* [x] RGB 16 565
			* [x] RGB 30
	+ [x] Data.CairoImage
		- [x] structure
		- [x] others
* [x] refactor System.TargetEndian
	+ [x] export list
	+ [x] import list
	+ [x] structure
		- [x] remove function `foo`
	+ [x] body
		- [x] function `endian`
		- [x] `data Endian`
		- [x] function `targetEndian`
		- [x] function `checkEndian`
* [ ] refactor Data.CairoImage.Internal
	+ [x] export list
	+ [x] import list
	+ [x] structure
		- [x] rough
		- [x] detailed
			* [x] ARGB 32
				+ [x] Pixel
				+ [x] Image
				+ [x] Image Mutable
			* [x] RGB 24
				+ [x] Pixel
				+ [x] Image
				+ [x] Image Mutable
			* [x] A 8
				+ [x] Pixel
				+ [x] Image
				+ [x] Image Mutable
			* [x] A 1
				+ [x] Pixel
				+ [x] Image
				+ [x] Image Mutable
			* [x] RGB 16 565
				+ [x] Pixel
				+ [x] Image
				+ [x] Image Mutable
			* [x] RGB 30
				+ [x] PIXEL
				+ [x] IMAEG
				+ [x] IMAEG MUTABLE
		- [x] make section COMMON
		- [x] move function `gen` to section COMMON
	+ [ ] body
		- [x] CLASS IMAGE AND IMAGE MUTABLE
			* [x] `class Image`
			* [x] `class ImageMut`
		- [x] TYPE CAIRO IMAGE AND CAIRO IMAGE MUTABLE
			* [x] `data CairoImage`
			* [x] `instance Eq CairoImage`
			* [x] function `compareBytes`
			* [x] `data CairoImageMut`
			* [x] function `cairoImageFreeze`
			* [x] function `cairoImageThaw`
			* [x] function `cairoImageDataCopy`
		- [ ] IMAGE FORMAT
			* [x] ARGB 32
				+ [x] PIXEL
					- [x] `newtype PixelArgb32`
					- [x] `pattern PixelArgb32Premultiplied`
					- [x] function `pixelArgb32Premultiplied`
					- [x] function `pixelArgb32FromArgb`
					- [x] function `pixelArgb32ToArgb`
					- [x] `pattern PixelArgb32Straight`
					- [x] function `pixelArgb32ToArgbSt`
					- [x] function `unit`
					- [x] function `div'`
				+ [x] IMAGE
					- [x] `data Argb32`
					- [x] `pattern CairoImageArgb32`
					- [x] function `cairoImageToArgb32`
					- [x] `instance Image Argb32`
				+ [x] IMAGE MUTABLE
					- [x] `data Argb32Mut s`
					- [x] `pattern CairoImageMutArgb32`
					- [x] function `cairoImageMutToArgb32`
					- [x] `instance ImageMut Argb32Mut`
			* [x] RGB 24
				+ [x] PIXEL
					- [x] `newtype PixelRgb24`
					- [x] `pattern PixelRgb24`
					- [x] function `pixelRgb24ToRgb`
				+ [x] IMAGE
					- [x] `data Rgb24`
					- [x] `pattern CairoImageRgb24`
					- [x] function `cairoImageToRgb24`
					- [x] `instance Image Rgb24`
				+ [x] IMAGE MUTABLE
					- [x] `data Rgb24Mut s`
					- [x] `pattern CairoImageMutRgb24`
					- [x] function `cairoImageMutToRgb24`
					- [x] `instance ImageMut Rgb24Mut`
			* [x] A 8
				+ [x] Pixel
					- [x] `newtype PixelA8`
				+ [x] Image
					- [x] `data A8`
					- [x] `pattern CairoImageA8`
					- [x] function `cairoImageToA8`
					- [x] `instance Image A8`
				+ [x] Image Mutable
					- [x] `data A8Mut s`
					- [x] `pattern CairoImageMutA8`
					- [x] function `cairoImageMutToA8`
					- [x] `instance ImageMut A8Mut`
			* [ ] A 1
				+ [x] PIXEL
					- [x] `newtype PixelA1`
					- [x] `data Bit`
					- [x] function `bit`
					- [x] function `ptrA1`
					- [x] function `peekA1`
					- [x] function `pokeA1`
				+ [x] IMAGE
					- [x] `data A1`
					- [x] `pattern CairoImageA1`
					- [x] function `cairoImageToA1`
					- [x] `instance Image A1`
					- [x] function `genA1`
				+ [ ] IMAGE MUTABLE
					- [ ] `data A1Mut s`
					- [ ] `pattern CairoImageMutA1`
					- [ ] function `cairoImageMutToA1`
					- [ ] `instance ImageMut A1Mut`
					- [ ] function `newA1Mut`
			* [ ] RGB 16 565
				+ [ ] Pixel
					- [ ] `newtype PixelRgb16_565`
					- [ ] function `ptrRgb16_565`
					- [ ] `pattern PixelRgb16_565`
					- [ ] function `pixelRgb16_565FromRgb`
					- [ ] function `pixelRgb16_565ToRgb`
				+ [ ] Image
					- [ ] `data Rgb16_565`
					- [ ] `pattern CairoImageRgb16_565`
					- [ ] function `cairoImageToRgb16_565`
					- [ ] `instance Image Rgb16_565`
					- [ ] function `generateRgb16_565PrimM`
				+ [ ] Image Mutable
					- [ ] `data Rgb16_565Mut s`
					- [ ] `pattern CairoImageMutRgb16_565`
					- [ ] function `cairoImageMutToRgb16_565`
					- [ ] `instance ImageMut Rgb16_565Mut`
					- [ ] function `newRgb16_565Mut`
			* [ ] RGB 30
				+ [ ] PIXEL
					- [ ] `newtype PixelRgb30`
					- [ ] function `ptrRgb30`
					- [ ] `pattern PixelRgb30`
					- [ ] function `pixelRgb30FromRgb`
					- [ ] function `pixelRgb30ToRgb`
				+ [ ] IMAGE
					- [ ] `data Rgb30`
					- [ ] `pattern CairoImageRgb30`
					- [ ] function `cairoImageToRgb30`
					- [ ] `instance Image Rgb30`
					- [ ] function `generateRgb30PrimM`
				+ [ ] IMAGE MUTABLE
					- [ ] `data Rgb30Mut s`
					- [ ] `pattern CairoImageMutRgb30`
					- [ ] function `cairoImageMutToRgb30`
					- [ ] `instance ImageMut Rgb30Mut`
					- [ ] function `newRgb30Mut`
	+ [ ] COMMON
* [ ] refactor Data.CairoImage

test
----

* [x] test `endian`

enhancement
-----------

* [x] add RGB 16 565
	+ [x] `data PixelRgb16_565`
	+ [x] `pattern PixelRgb16_565`
		- [x] function `pixelRgb16_565FromRgb`
		- [x] function `pixelRgb16_565ToRgb`
		- [x] COMPLETE pragma
	+ [x] repair function `pixelRgb16_565ToRgb`
	+ [x] `data Rgb16_565`
		- [x] define data type
		- [x] instance Image
	+ [x] `data Rgb16_565Mut`
		- [x] define data type
		- [x] instance ImageMut
	+ [x] `pattern CairoImageRgb16_565`
	+ [x] `pattern CairoImageMutRgb16_565`
* [x] add RGB 30
	+ [x] `data PixelRgb30`
	+ [x] `pattern PixelRgb30`
		- [x] function `pixelRgb30FromRgb`
		- [x] function `pixelRgb30ToRgb`
		- [x] `pattern PixelRgb30`
		- [x] COMPLETE pragma
	+ [x] test `pattern PixelRgb30`
	+ [x] `data Rgb30`
		- [x] define data type
		- [x] instance Image
	+ [x] `data Rgb30Mut`
		- [x] define data type
		- [x] instance ImageMut
	+ [x] `pattern CairoImageRgb30`
	+ [x] `pattern CairoImageMutRgb30`

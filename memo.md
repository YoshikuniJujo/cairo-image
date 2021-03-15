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
	+ [ ] structure
		- [x] rough
		- [ ] detailed
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
			* [ ] RGB 16 565
			* [ ] RGB 30
	+ [ ] body
		- [ ] CLASS IMAGE AND IMAGE MUTABLE
		- [ ] TYPE CAIRO IMAGE AND CAIRO IMAGE MUTABLE
		- [ ] IMAGE FORMAT
			* [ ] ARGB 32
				+ [ ] Pixel
					- [ ] `newtype PixelArgb32`
					- [ ] function `ptrArgb32`
					- [ ] `pattern PixelArgb32Premultiplied`
					- [ ] `pattern PixelArgb32Straight`
				+ [ ] Image
					- [ ] `data Argb32`
					- [ ] `pattern CairoImageArgb32`
					- [ ] function `cairoImageToArgb32`
					- [ ] `instance Image Argb32`
					- [ ] function `generateArgb32PrimM`
				+ [ ] Image Mutable
					- [ ] `data Argb32Mut s`
					- [ ] `pattern CairoImageMutArgb32`
					- [ ] function `cairoImageMutToArgb32`
					- [ ] `instance ImageMut Argb32Mut`
					- [ ] function `newArgb32Mut`
			* [ ] RGB 24
				+ [ ] Pixel
					- [ ] `newtype PixelRgb24`
					- [ ] function `ptrRgb24`
					- [ ] `pattern PixelRgb24`
					- [ ] function `pixelRgb24ToRgb`
				+ [ ] Image
					- [ ] `data Rgb24`
					- [ ] `pattern CairoImageRgb24`
					- [ ] function `cairoImageToRgb24`
					- [ ] `instance Image Rgb24`
					- [ ] function `generateRgb24PrimM`
				+ [ ] Image Mutable
					- [ ] `data Rgb24Mut s`
					- [ ] `pattern CairoImageMutRgb24`
					- [ ] function `cairoImageMutToRgb24`
					- [ ] `instance ImageMut Rgb24Mut`
					- [ ] function `newRgb24Mut`
			* [ ] A 8
				+ [ ] Pixel
					- [ ] `newtype PixelA8`
					- [ ] function `ptrA8`
				+ [ ] Image
					- [ ] `data A8`
					- [ ] `pattern CairoImageA8`
					- [ ] function `cairoImageToA8`
					- [ ] `instance Image A8`
					- [ ] function `generateA8PrimM`
				+ [ ] Image Mutable
					- [ ] `data A8Mut s`
					- [ ] `pattern CairoImageMutA8`
					- [ ] function `cairoImageMutToA8`
					- [ ] `instance ImageMut A8Mut`
					- [ ] function `newA8Mut`
			* [ ] A 1
				+ [ ] Pixel
					- [ ] `newtype PixelA1`
					- [ ] `data Bit`
					- [ ] function `bit`
					- [ ] function `ptrA1`
					- [ ] function `peekA1`
					- [ ] function `pokeA1`
				+ [ ] Image
					- [ ] `data A1`
					- [ ] `pattern CairoImageA1`
					- [ ] function `cairoImageToA1`
					- [ ] `instance Image A1`
					- [ ] function `generateA1PrimM`
				+ [ ] Image Mutable
					- [ ] `data A1Mut s`
					- [ ] `pattern CairoImageMutA1`
					- [ ] function `cairoImageMutToA1`
					- [ ] `instance ImageMut A1Mut`
					- [ ] function `newA1Mut`
			* [ ] RGB 16 565
			* [ ] RGB 30
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

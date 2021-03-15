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
			* [ ] ARGB 32
				+ [x] Pixel
				+ [x] Image
				+ [ ] Image Mutable
			* [ ] RGB 24
			* [ ] A 8
			* [ ] A 1
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
			* [ ] RGB 24
			* [ ] A 8
			* [ ] A 1
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

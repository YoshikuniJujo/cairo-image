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
* [ ] repair `pattern PixelArgb32`
	+ [x] function `unit`
	+ [x] make `pattern PixelArgb32Straight`
		- [x] function `pixelArgb32FromArgbStrait`
		- [x] function `pixelArgb32ToArgbStrait`
		- [x] `pattern PixelArgb32Straight`
	+ [x] make `pattern PixelArgb32Premultiplied`
	+ [x] make function `pixelArgb32Premultiplied`
		- `Word8 -> Word8 -> Word8 -> Word8 -> Maybe PixelArgb32`
	+ [x] remove `pattern PixelArgb32`
	+ [ ] repiar JuicyCairo
	+ [ ] repair try-cairo

refactoring
-----------

* [ ] refactor document
	+ [x] System.TargetEndian
	+ [ ] Data.CairoImage.Internal
		- [x] structure
		- [ ] Class Image and ImageMut
			* [ ] class Image
			* [ ] class ImageMut
		- [ ] Type CairoImage and CairoImageMut
		- [ ] Image Format
			* [ ] ARGB 32
			* [ ] RGB 24
			* [ ] A 8
			* [ ] A 1
			* [ ] RGB 16 565
			* [ ] RGB 30
	+ [ ] Data.CairoImage
* [ ] refactor System.TargetEndian
* [ ] refactor Data.CairoImage.Internal
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

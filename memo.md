memo
====

repair
------

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
		- [ ] structure
		- [ ] Class Image and ImageMut
		- [ ] Type CairoImage and CairoImageMut
		- [ ] Image Format
			* [ ] ARGB 32
			* [ ] RGB 24
			* [ ] A 8
			* [ ] A 1
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
		- [x] instance Image
	+ [x] `pattern CairoImageRgb16_565`
	+ [x] `pattern CairoImageMutRgb16_565`
* [ ] add RGB 30
	+ [x] `data PixelRgb30`
	+ [ ] `pattern PixelRgb30`
	+ [ ] `data Rgb30`
	+ [ ] `data Rgb30Mut`
	+ [ ] `pattern CairoImageRgb30`
	+ [ ] `pattern CairoImageMutRgb30`

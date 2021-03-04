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
	+ [ ] make function `pixelArgb32Premultiplied`
		- `Word8 -> Word8 -> Word8 -> Word8 -> Maybe PixelArgb32`
	+ [ ] remove `pattern PixelArgb32`
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

* [ ] add RGB 16 565
* [ ] add RGB 30

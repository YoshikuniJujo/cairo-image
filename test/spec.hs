{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Data.CairoImage.Internal

main :: IO ()
main = do
	let	rgb16max = PixelRgb16_565 0xff 0xff 0xff
		PixelRgb16_565 ra ga ba = rgb16max
		rgb16min = PixelRgb16_565 0x00 0x00 0x00
		PixelRgb16_565 ri gi bi = rgb16min
		rgb16foo = PixelRgb16_565 0x12 0x34 0x56
		PixelRgb16_565 rf gf bf = rgb16foo
	print rgb16max
	print (ra, ga, ba)
	print rgb16min
	print (ri, gi, bi)
	print rgb16foo
	print (rf, gf, bf)
	print (0x12 :: Word8, 0x34 :: Word8, 0x56 :: Word8)

import Data.CairoImage.Internal

main :: IO ()
main = do
	let	rgb16max = PixelRgb16_565 0xff 0xff 0xff
	let	PixelRgb16_565 r g b = rgb16max
	print rgb16max
	print (r, g, b)

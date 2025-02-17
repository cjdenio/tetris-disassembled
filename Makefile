IMAGE_DEPS = gfx/image_000_3e87.2bpp

all: build/game.gb

%.2bpp: %.png
	rgbgfx -o $@ $<

%.1bpp: %.png
	rgbgfx -d 1 -o $@ $<

build/game.o: tetris.asm tiles.asm charmap.inc hardware.inc $(IMAGE_DEPS)
	rgbasm -o $@ -p 255 tetris.asm

build/game.gb: build/game.o
	rgblink -t -n build/game.sym -m build/game.map -o $@ $<
	rgbfix -v -p 255 $@

	shasum $@

clean:
	rm -f build/*
	# find . \( -iname '*.1bpp' -o -iname '*.2bpp' \) -exec rm {} +

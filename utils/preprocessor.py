import sys
import re

# TODO clean up this enormous mess.

from PIL import Image

DEFAULTFILE = "../pong.asm"

SINGLE_LINE_MACRO_REGEX = r"([^ ]+) +{{ *(.+) *}}$"

MULTI_LINE_START_MACRO_REGEX = r"^([^ ]+) +{{ *(.+)? *$"
MULTI_LINE_MID_MACRO_REGEX = r"^ *([^}]+) *$"
MULTI_LINE_END_MACRO_REGEX = r"^ *([^}]+)? *}}$"

SEEN_TILES = []
SEEN_BG_TILES = []

# for looking up palette indices.
SEEN_TILES_MAP = {}
BG_TILE_MAP = {}

# reuse the same function for tiles (which can have four colors),
# because why not.
# the code isn't different.
def sprites(*args, root="sprites", is_bg=False):
    sprites = []
    palettes = []
    for arg in args:
        s, p = sprite(path=arg, root=root, is_bg=is_bg)
        sprites.append(s)
        palettes.append(p)

    return ("\n".join(sprites) + "\n", finish_palettes(palettes))

def finish_palettes(palettes):
    # pad out to 32 colors;
    for i in range(len(palettes), 32, 4):
        palettes.append(
            "\n".join([
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "",
            ]))

    return "\n".join(palettes) + "\n"

def extract_palette_colors(img):
    """extract palette colors from a paletted png."""
    pal =  img.palette.palette

    asm_colors = []
    # TODO I don't quite understand what this first loop is doing.
    for i in range(0, len(pal), 12):
        for j in range(i, i + 12, 3):
            try:
                r, g, b = color = pal[j:j + 3]
            except ValueError:
                r, g, b = 255, 255, 255

            # Convert to RGB555
            asm_colors.append((r >> 3) | ((g >> 3) << 5) | ((b >> 3) << 10))

    return asm_colors

def sprite(*, path, root, is_bg):
    img = Image.open(f"data/{root}/{path}.png")

    if img.palette is None:
        print("This has been modified to only work on paletted PNGs!", file=sys.stderr)
        sys.exit(1)

    SEEN_TILES.append(path.upper())
    SEEN_TILES_MAP[path] = len(SEEN_TILES)-1

    if is_bg:
        SEEN_BG_TILES.append(path.upper())
        BG_TILE_MAP[path] = len(SEEN_BG_TILES)-1

    palette = []
    asm_colors = extract_palette_colors(img)
    for asm_color in asm_colors:
        palette.append(f"    dw %{asm_color:016b}")

    sprite = []
    bytes1, bytes2 = [], []
    col = 0
    # these are paletted PNGs, so these already reference the palette and we just
    # have to convert to binary.
    for pixels in list(img.getdata()):

        if col == 0:
            bytes1.append([])
            bytes2.append([])

        binstring = format(pixels % 4, "02b")
        bytes1[-1].append(binstring[1])
        bytes2[-1].append(binstring[0])

        col += 1
        if col > 7:
            col = 0

    for byte1, byte2 in zip(bytes1, bytes2):
        sprite.append("DB %" + "".join(byte1) + ",%" + "".join(byte2))

    return "\n".join(sprite) + "\n", "\n".join(palette) + "\n"

def define_map(*, tileset, mapfile):
    """
    define a map in bytes with db.
    read pixels, and replace with constants for each tile.
    also define map palettes.
    """
    img = Image.open(f"data/maps/{mapfile}.png")

    # GB background dimensions.
    tile_width = 32

    map_rows = []
    map_row_bytes = []

    palette_rows = []
    palette_row_bytes = []

    width = img.width
    col = 0
    for i, pixel in enumerate(img.getdata()):
        if col == width:
            # don't forget to pad with 0 bytes.
            if len(map_row_bytes) < tile_width:
                map_row_bytes.extend(["0"] * (tile_width-len(map_row_bytes)))
                palette_row_bytes.extend(["`00000000"] * (tile_width-len(palette_row_bytes)))

            map_rows.append("    db " + ", ".join(map_row_bytes))
            map_row_bytes = []

            palette_rows.append("    db " + ", ".join(palette_row_bytes))
            palette_row_bytes = []
            col = 0

        # default to 0, so it's visible when you've messed up.
        if pixel >= len(tileset):
            label = "0"

            palette = 0

        else:
            label = f"{SEEN_TILES_MAP[tileset[pixel]]}"

            palette = f"{BG_TILE_MAP[tileset[pixel]]:03b}"

        map_row_bytes.append(label)

        # flip across midline
        if col >= 10:
            flip_bit = 1
        else:
            flip_bit = 0

        palette_row_bytes.append(f"`00{flip_bit}00{palette}")

        col += 1

    # don't forget last row.
    if len(map_row_bytes) < tile_width:
        map_row_bytes.extend(["0"] * (tile_width-len(map_row_bytes)))
        palette_row_bytes.extend(["`00000000"] * (tile_width-len(palette_row_bytes)))

    map_rows.append("    db " + ", ".join(map_row_bytes))
    palette_rows.append("    db " + ", ".join(palette_row_bytes))

    return ("\n".join(map_rows) + "\n", "\n".join(palette_rows) + "\n")

# Just echos back what you put in, for testing
def echo(thing):
    return thing

def process(inname, outname=None):
    output_lines = []

    # store palettes after sprites generated.
    # kinda gross, but imo less gross than doing two passes where we put a macro expression in
    # both times, or something.
    sprite_palettes = []
    tile_palettes = []

    # set map palettes.
    map_tile_palettes = []
    with open(inname, "r") as f:
        line_i = 0
        lines = f.readlines()
        while line_i < len(lines):
            line = lines[line_i]

            label = None
            expression = None

            new_line = None

            # single-line match.
            match = re.match(SINGLE_LINE_MACRO_REGEX, line)
            if match:
                label = match.group(1)
                expression = match.group(2)

                line_i += 1
                line = lines[line_i]

            # multi-line match.
            if not label:
                match = re.match(MULTI_LINE_START_MACRO_REGEX, line)
                if match:
                    label = match.group(1)
                    expression = match.group(2) if match.group(2) else ""

                    line_i += 1
                    line = lines[line_i]

                    # could be any number of mid matches.
                    while (match):
                        match = re.match(MULTI_LINE_MID_MACRO_REGEX, line)
                        if match:
                            expression += match.group(1) if match.group(1) else ""
                            line_i += 1
                            line = lines[line_i]

                    # when match fails, do end match.
                    match = re.match(MULTI_LINE_END_MACRO_REGEX, line)
                    if match:
                        expression += match.group(1) if match.group(1) else ""
                        line_i += 1
                        line = lines[line_i]

                        expression = expression.replace("\n", "")

            if label and expression:
                # well, this is gross.
                if label == "Sprites:":
                    # Eval whatever they put in the macro
                    bin_sprites, sprite_palettes = eval(expression)

                    # Rebuild the line with the new result
                    new_line = f"{label}\n{bin_sprites}"

                elif label == "Tiles:":
                    bin_tiles, tile_palettes = eval(expression)
                    new_line = f"{label}\n{bin_tiles}"

                elif label == "SpritePalettes:":
                    new_line = f"{label}\n{sprite_palettes}"

                elif label == "TilePalettes:":
                    new_line = f"{label}\n{tile_palettes}"

                elif label == "ObjectConstants:":
                    new_line = "; tile constants for map reference\n"
                    new_line += "\n".join(
                        [f"{tile} EQU {i}" for i, tile in enumerate(SEEN_TILES)]
                    )

                elif label == "Map:":
                    bin_map, map_tile_palettes = eval(expression)
                    new_line = f"{label}\n{bin_map}"

                elif label == "MapData:":
                    new_line = f"{label}\n{map_tile_palettes}"

                output_lines.append(new_line)

            else:
                output_lines.append(line)

            # make sure to update index properly,
            # but only if we didn't find a match
            # (because those cases handle their own increment)
            if not label:
                line_i += 1

                if line_i == len(lines):
                    break

                line = lines[line_i]


    with open(outname or inname + ".built", "w") as f:
        f.write("".join(output_lines))

def main():
    if len(sys.argv) != 2 and len(sys.argv) != 3:
        print("Please provide a single argument pointing to the .asm you want to process")
        print("Optionally you may provide a second argument specifying the output file name")
        print(f"Assuming{DEFAULTFILE} for testing purposes")

    process(sys.argv[1] if len(sys.argv) > 1 else DEFAULTFILE,
            sys.argv[2] if len(sys.argv) > 2 else None)

if __name__ == "__main__":
    main()

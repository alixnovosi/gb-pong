import collections
import re
import sys

from PIL import Image

# TODO clean up this enormous mess.

# CONSTANTS
DEFAULTFILE = "../pong.asm"

SINGLE_LINE_MACRO_REGEX = r"^([A-Za-z0-9:]+) +{{ *([^}}\n]*[^}}\n ]*) *}}$"

MULTI_LINE_START_MACRO_REGEX = r"^([A-Za-z0-9:]+) +{{ *([^}}\n]*[^}}\n ]*) *"
MULTI_LINE_MID_MACRO_REGEX = r"^\s*([^{}\s#]*[\(\\)\[\],]+)( *.*)?$"
MULTI_LINE_END_MACRO_REGEX = r"^ *}} *$"

BG_TYPE = "BG"
SPRITE_TYPE = "SPRITE"

# data used for constructing palettes and tiles.
OUTPUT_TILES = ""

# "TileName" -> {"tile_index": 1, "palette_index": 2}
NAME_TO_TILE_DATA = collections.OrderedDict()

# these store actual palette dw chunks.
OUTPUT_SPRITE_PALETTES = ""
OUTPUT_BG_PALETTES = ""

# map bytes (indicating what tiles) and data bytes (indicating palettes)
OUTPUT_MAP = []
OUTPUT_MAP_DATA = []

# named tuples used in calls to function.

# Tile - represents a tile, which is either a background square or a sprite.
#     (or part of the window, but I'm not using that yet).
# name - filename
# palette - whether to create new palette ('*'),
#     or to reuse another palette (referred to with filename).
# type - sprite or BG, determines where palette goes. also determines dir.
# palette_only - for sprites where the image is thrown away,
#     but the palette should be saved.
Tile = collections.namedtuple(
    "Tile",
    ("name", "palette", "type", "palette_only"),
    defaults=(None, None, None, None),
)

# MapTile - just a container for the two things we need for the map,
#     a reference to a tile, and the palette to use for that tile.
#     The map function call will combine this with a map file specifying where squares go,
#     and then magic turns that into the two BG maps the gameboy needs.
# tile - reference to Tile we should use (will fetch bin data for it).
# palette - reference to palette we should use.
MapTile = collections.namedtuple(
    "MapTile",
    ("tile", "palette"),
    defaults=(None, None),
)

def define_tiles(*, tiles):
    """define several tiles."""
    output_tiles_list = []
    output_sprite_palettes_list = []
    output_bg_palettes_list = []

    for tile in tiles:
        sprite, palette = extract_tile(tile)

        dict_entry = {}

        # handle tiles which are palette only.
        if not tile.palette_only:
            output_tiles_list.append(sprite)
            dict_entry["tile_index"] = len(output_tiles_list)-1

        if tile.palette != "*":
            dict_entry["palette_index"] = NAME_TO_TILE_DATA[tile.palette]["palette_index"]

        # new palette means a bunch of bookkeeping.
        else:
            if tile.type == BG_TYPE:
                output_bg_palettes_list.append(palette)
                dict_entry["palette_index"] = len(output_bg_palettes_list)-1

            else:
                output_sprite_palettes_list.append(palette)
                dict_entry["palette_index"] = len(output_sprite_palettes_list)-1

        NAME_TO_TILE_DATA[tile.name] = dict_entry

    return (
        "\n\n".join(output_tiles_list) + "\n",
        finish_palettes(output_bg_palettes_list),
        finish_palettes(output_sprite_palettes_list),
    )

def extract_tile(tile_tuple):
    """pull out bin image and palette for a tile"""
    path = tile_tuple.name

    if tile_tuple.type == BG_TYPE:
        root = "tiles"
    else:
        root = "sprites"

    img = Image.open(f"data/{root}/{path}.png")
    if img.palette is None:
        print("This has been modified to only work on paletted PNGs!", file=sys.stderr)
        sys.exit(1)

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
        sprite.append("    db %" + "".join(byte1) + ",%" + "".join(byte2))

    return "\n".join(sprite), "\n".join(palette)

def finish_palettes(palettes):
    # pad out to 8 palettes
    for i in range(8-len(palettes)):
        palettes.append(
            "\n".join([
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "    dw 0",
            ]))

    print(f"finish func has produced {palettes}")

    res = "\n\n".join(palettes) + "\n"

    print(f"and for some reason we produce {res}")

    return res

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

def define_font(*, fontfile):
    """
    define a font in bytes with db.

    font given as a single PNG with sentinel pixels to indicate char breaks.
    """
    img = Image.open(f"data/{fontfile}")
    fontimg =  img.palette.palette

    height = img.height
    width = img.width

    # read sentinel values to get starts/ends of chars.
    # sentinel values are assumed to be in the first row,
    # with one blank row,
    # and then letter data.
    bounds = []
    data = img.getdata()
    last_filled_index = None
    current_pair = []
    for i in range(img.width):
        pixel = data[i]

        if pixel == 1:
            if len(current_pair) > 0:
                current_pair.append(i-2)
                bounds.append(current_pair)
                current_pair = [i]
            else:
                current_pair.append(i)

            # take two adjacent sentinel pixels as a sign to stop
            if last_filled_index is not None and last_filled_index - i == 1:
                break

            last_filled_index = i

    # grab letter data and generate db blocks to be "\n".join'd
    letters = []
    for i, [start, end] in enumerate(bounds):
        letter = [
            f"; alphabet item {i}",
            f"    db {end-start+1}",
        ]

        # we're just indexing into the 1D data array and pulling out letters here
        for r in range(2, height):
            row = []
            for c in range(start, end+1):
                row.append(str(data[r * width + c]))

            row.extend(["0"] * (8-len(row)))

            letter.append("    dw `" + "".join(row))

        letters.append("\n".join(letter) + "\n")

    return "\n".join(letters)


def define_map(*, tileset, mapfile):
    """
    define a map in bytes with db.
    read pixels, and replace with constants for each tile.
    also define map palettes.
    """
    img = Image.open(f"data/maps/{mapfile}.png")

    global OUTPUT_MAP
    global OUTPUT_MAP_DATA

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
            tileset_item = tileset[pixel]

            label = f"{NAME_TO_TILE_DATA[tileset_item.tile]['tile_index']}"

            palette = f"{NAME_TO_TILE_DATA[tileset_item.palette]['palette_index']:03b}"

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

    OUTPUT_MAP = "\n".join(map_rows) + "\n"
    OUTPUT_MAP_DATA = "\n".join(palette_rows) + "\n"

# Just echos back what you put in, for testing
def echo(thing):
    return thing

def process(inname, outname=None):
    global OUTPUT_SPRITE_PALETTES
    global OUTPUT_BG_PALETTES

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
                        line_i += 1
                        line = lines[line_i]

                        expression = expression.replace("\n", "")

            if label:
                # well, this is gross.
                if label == "Tiles:":
                    OUTPUT_TILES, OUTPUT_BG_PALETTES, OUTPUT_SPRITE_PALETTES = eval(expression)
                    new_line = f"{label}\n{OUTPUT_TILES}"

                    # debug info

                elif label == "SpritePalettes:":
                    new_line = f"{label}\n{OUTPUT_SPRITE_PALETTES}"

                elif label == "BGPalettes:":
                    new_line = f"{label}\n{OUTPUT_BG_PALETTES}"

                elif label == "ObjectConstants:":
                    new_line = "; tile constants for map reference\n"
                    new_line += "\n".join(
                        [
                            f"{key.upper()} EQU {val['tile_index']}"
                            for key, val
                            in NAME_TO_TILE_DATA.items()
                            if "tile_index" in val
                        ]
                    )

                elif label == "Map:":
                    eval(expression)
                    new_line = f"{label}\n{OUTPUT_MAP}"

                elif label == "MapData:":
                    new_line = f"{label}\n{OUTPUT_MAP_DATA}"

                elif label == "Font:":
                    font = eval(expression)
                    new_line = f"{label}\n{font}"

                if new_line is None:
                    print(f"we produced a none for {label}")
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

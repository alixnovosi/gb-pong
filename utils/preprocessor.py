import sys
import re

from PIL import Image

DEFAULTFILE = "../pong.asm"
MACROREGEX = r"([^ ]+) +{{ *(.+) *}}"

def tiles(*args):
    tiles = []
    for arg in args:
        t, p = tile(arg)
        tile.append(t)
        palettes.append(p)

    # pad out to 32 colors;
    for i in range(len(palettes), 32, 4):
        palettes.append(
            "\n".join([
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "",
            ])
        )

    return ("\n".join(tiles) + "\n", "\n".join(palettes) + "\n")

def sprites(*args):
    sprites = []
    palettes = []
    for arg in args:
        s, p = sprite(arg)
        sprites.append(s)
        palettes.append(p)

    # pad out to 32 colors;
    for i in range(len(palettes), 32, 4):
        palettes.append(
            "\n".join([
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "    dw 0",
                "",
            ])
        )

    return ("\n".join(sprites) + "\n", "\n".join(palettes) + "\n")

def sprite(path):
    img = Image.open(f"sprites/{path}.png")

    if img.palette is None:
        print("This has been modified to only work on paletted PNGs!", file=sys.stderr)
        sys.exit(1)

    palette = []
    sprite = []
    pal =  img.palette.palette
    # TODO I don't quite understand what this first loop is doing.
    for i in range(0, len(pal), 12):
        for j in range(i, i + 12, 3):
            try:
                r, g, b = color = pal[j:j + 3]
            except ValueError:
                r, g, b = 255, 255, 255

            # Convert to RGB555
            asm_color = (r >> 3) | ((g >> 3) << 5) | ((b >> 3) << 10)

            palette.append(f"    dw %{asm_color:016b}")

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

# Just echos back what you put in, for testing
def echo(thing):
    return thing

def process(inname, outname=None):
    output_lines = []

    # store palettes after sprites generated.
    # kinda gross, but imo less gross than doing two passes where we put a macro expression in
    # both times, or something.
    palettes = ""
    with open(inname, "r") as f:
        for line in f.readlines():
            new_line = None
            match = re.match(MACROREGEX, line)
            if match:
                label = match.group(1)
                expression = match.group(2)

                # well, this is gross.
                if label == "Sprites:":
                    # Eval whatever they put in the macro
                    sprites, palettes = eval(expression)

                    # Rebuild the line with the new result
                    line = f"{label}\n{sprites}"

                else:
                    line = f"{label}\n{palettes}"

            output_lines.extend(new_line or [line])

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

import sys

# nothing yet
def process(inname, outname=None):
    output_lines = []
    with open(inname, "r") as inf:
        with open(outname or inname + ".built", "w") as outf:
            outf.write(inf.read())

def main():
    if len(sys.argv) != 2 and len(sys.argv) != 3:
        print("Please provide a single argument pointing to the .asm you want to process")
        print("Optionally you may provide a second argument specifying the output file name")
        print(f"Assuming {DEFAULTFILE} for testing purposes")

    process(sys.argv[1] if len(sys.argv) > 1 else DEFAULTFILE,
            sys.argv[2] if len(sys.argv) > 2 else None)

if __name__ == '__main__':
    main()

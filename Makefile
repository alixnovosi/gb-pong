PYTHON=python3
VIRTUALENV=. venv/bin/activate

PREPROC=$(VIRTUALENV); $(PYTHON) utils/preprocessor.py

ASM=rgbasm -iincludes/
LINK=rgblink
FIX=rgbfix -C

TYPE=gbc

EMU=binjgb
DEBUG=binjgb-debugger

TARGET=pong

.PHONY: run clean

all: build run

build: preprocess asm link fix

preprocess: $(TARGET).asm
	$(PREPROC) $(TARGET).asm $(TARGET).asm.built

asm: $(TARGET).asm.built
	$(ASM) -o$(TARGET).obj $(TARGET).asm.built

link: $(TARGET).obj
	$(LINK) -o$(TARGET).$(TYPE) $(TARGET).obj

fix: $(TARGET).$(TYPE)
	$(FIX) -v -p0 $(TARGET).$(TYPE)

run:
	$(EMU) $(TARGET).$(TYPE)

debug:
	$(DEBUG) $(TARGET).$(TYPE)

clean:
	$(RM) $(TARGET).obj $(TARGET).$(TYPE) $(TARGET).asm.built

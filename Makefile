## Output Configuration
OUTPUT_DIR    = bin
OUTPUT_NAME   = olec
OUTPUT_TARGET = $(OUTPUT_DIR)/$(OUTPUT_NAME)

## Haskell Source Configuration
SOURCE_DIR    = src
SOURCE_MAIN   = src/Main.hs
SOURCE_FILES  = $(shell find $(SOURCE_DIR) -iname "*.hs" -type f)

## C Source Configuration
EXTERN_DIR    = ext
EXTERN_OBJS   = bin/terminal.o

## Compilation Flags
GHCFLAGS      = -j6 -W -O3 -threaded -outputdir $(OUTPUT_DIR) -i$(SOURCE_DIR) -lncursesw
CFLAGS        = -std=c11 -fmessage-length=0 -Wall -Wpedantic -O3 -fPIC
RUNFLAGS      =

## Programs
GHC           = ghc
CC            = gcc
RM            = rm -rf

## Common Targets
all: $(OUTPUT_TARGET)

clean:
	$(RM) $(OUTPUT_DIR)/*

run: $(OUTPUT_TARGET)
	$(shell realpath $(OUTPUT_TARGET)) $(RUNFLAGS)

## C Targets
$(OUTPUT_DIR)/%.o: $(EXTERN_DIR)/%.c
	$(CC) -c $(CFLAGS) -o$@ $<

## Haskell Targets
$(OUTPUT_TARGET): $(EXTERN_OBJS) $(SOURCE_FILES)
	$(GHC) --make $(GHCFLAGS) -o $@ $(SOURCE_MAIN) $(EXTERN_OBJS)

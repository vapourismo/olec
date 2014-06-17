## Configuration
PROJECT_NAME      = olec
OUTPUT_DIR        = bin

## Library Configuration
LIB_NAME          = $(PROJECT_NAME)
LIB_SOURCE_DIR    = lib
LIB_HEADER_DIR    = $(LIB_SOURCE_DIR)
LIB_SOURCE_FILES  = $(shell find $(LIB_SOURCE_DIR) -type 'f' -iname '*.c')
LIB_HEADER_FILES  = $(shell find $(LIB_SOURCE_DIR) -type 'f' -iname '*.h')

LIB_STATIC_OUTPUT = $(OUTPUT_DIR)/lib$(LIB_NAME).a
LIB_SHARED_OUTPUT = $(OUTPUT_DIR)/lib$(LIB_NAME).so
LIB_OBJECT_FILES  = $(LIB_SOURCE_FILES:%.c=%.o)
LIB_DEPEND_FILES  = $(LIB_SOURCE_FILES:%.c=%.d)

## Executable Configuration
EXE_NAME          = $(PROJECT_NAME)-test
EXE_SOURCE_DIR    = src
EXE_SOURCE_FILES  = $(shell find $(EXE_SOURCE_DIR) -type 'f' -iname '*.c')

EXE_OUTPUT        = $(OUTPUT_DIR)/$(EXE_NAME)
EXE_OBJECT_FILES  = $(EXE_SOURCE_FILES:%.c=%.o)
EXE_DEPEND_FILES  = $(EXE_SOURCE_FILES:%.c=%.d)

## Flags
LIB_CC_FLAGS      = -std=c11 -fPIC -fmessage-length=0 -Wall -Wpedantic -O0 -g -DDEBUG
LIB_LD_FLAGS      = -shared -lncurses -lm
LIB_AR_FLAGS      = rcs
EXE_CC_FLAGS      = -std=c11 -fmessage-length=0 -Wall -Wpedantic -O0 -g -DDEBUG
EXE_LD_FLAGS      = -lncurses -lm

## Programs
CC                = clang
LD                = clang
AR                = ar
RM                = rm -rf
MKDIR             = mkdir -p

## Main Targets
all: $(LIB_SHARED_OUTPUT)

static: $(LIB_STATIC_OUTPUT)

shared: $(LIB_SHARED_OUTPUT)

test: $(EXE_OUTPUT)
	$(shell realpath $(EXE_OUTPUT))

clean:
	$(RM) $(LIB_STATIC_OUTPUT) $(LIB_SHARED_OUTPUT) $(LIB_OBJECT_FILES) $(LIB_DEPEND_FILES)
	$(RM) $(EXE_OUTPUT) $(EXE_DEPEND_FILES) $(EXE_OBJECT_FILES)

## Include Dependencies
-include $(LIB_DEPEND_FILES)
-include $(EXE_DEPEND_FILES)

## Compilation
$(LIB_SOURCE_DIR)/%.o: $(LIB_SOURCE_DIR)/%.c
	$(CC) -c $(LIB_CC_FLAGS) -MMD -MF$(@:%.o=%.d) -MT$@ -o$@ $<

$(EXE_SOURCE_DIR)/%.o: $(EXE_SOURCE_DIR)/%.c
	$(CC) -c $(EXE_CC_FLAGS) -MMD -MF$(@:%.o=%.d) -MT$@ -o$@ $<

## Directories
$(OUTPUT_DIR):
	$(MKDIR) $@

## Static Library
$(LIB_STATIC_OUTPUT): $(OUTPUT_DIR) $(LIB_OBJECT_FILES)
	$(AR) $(LIB_AR_FLAGS) $(LIB_STATIC_OUTPUT) $(LIB_OBJECT_FILES)

## Shared Library
$(LIB_SHARED_OUTPUT): $(OUTPUT_DIR) $(LIB_OBJECT_FILES)
	$(LD) $(LIB_LD_FLAGS) -o$(LIB_SHARED_OUTPUT) $(LIB_OBJECT_FILES)

## Test Executable
$(EXE_OUTPUT): $(OUTPUT_DIR) $(LIB_OBJECT_FILES) $(EXE_OBJECT_FILES)
	$(LD) $(EXE_LD_FLAGS) -o$(EXE_OUTPUT) $(EXE_OBJECT_FILES) $(LIB_OBJECT_FILES)

## Options
.PHONY: all clean shared static test

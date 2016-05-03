# Programs
RM    = rm -rf
CXX   = clang++
MKDIR = mkdir -p

# Files
SOURCE_PATH  = src
SOURCE_FILES = main.cc

OUTPUT_PATH = dist
OUTPUT_FILE = olec

BIN  = $(OUTPUT_PATH)/$(OUTPUT_FILE)
OBJS = $(SOURCE_FILES:%.cc=$(OUTPUT_PATH)/%.o)
DEPS = $(SOURCE_FILES:%.cc=$(OUTPUT_PATH)/%.d)
DIRS = $(dir $(OBJS))

# Flags
USECXXFLAGS = $(CXXFLAGS) -std=c++14 -fmessage-length=0 -Wall -Wextra -pedantic -D_GLIBCXX_USE_C99 \
              -O0 -g -DDEBUG
USELDFLAGS  = $(LDFLAGS)
USELDLIBS   = $(LDLIBS)

# Default Targets
all: $(BIN)

clean:
	$(RM) $(OUTPUT_PATH)

# Directories
$(DIRS):
	$(MKDIR) $@

# Dependencies
-include $(DEPS)

# C++ Targets
$(BIN): $(OBJS) $(DIRS)
	$(CXX) $(USELDFLAGS) -o$@ $(OBJS) $(USELDLIBS)

$(OUTPUT_PATH)/%.o: $(SOURCE_PATH)/%.cc $(DIRS)
	$(CXX) -c $(USECXXFLAGS) -o$@ -MMD -MF$(@:%.o=%.d) $<

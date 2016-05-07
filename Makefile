# Programs
RM    = rm -rf
CXX   = clang++
MKDIR = mkdir -p

# Files
SOURCE_PATH  = src
SOURCE_FILES = main.cpp termbox.cpp

OUTPUT_PATH = dist
OUTPUT_FILE = olec

BIN  = $(OUTPUT_PATH)/$(OUTPUT_FILE)
OBJS = $(SOURCE_FILES:%.cpp=$(OUTPUT_PATH)/%.o)
DEPS = $(SOURCE_FILES:%.cpp=$(OUTPUT_PATH)/%.d)
DIRS = $(dir $(OBJS))

# Flags
USECXXFLAGS = $(CXXFLAGS) -std=c++14 -D_GLIBCXX_USE_C99 -D_XOPEN_SOURCE \
              -O0 -g -DDEBUG -fmessage-length=0 -Wall -Wextra -pedantic
USELDFLAGS  = $(LDFLAGS)
USELDLIBS   = $(LDLIBS) -ltermbox -llua

# Default Targets
all: $(BIN)

gdb:
	gdb -ex run $(BIN)

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

$(OUTPUT_PATH)/%.o: $(SOURCE_PATH)/%.cpp $(DIRS)
	$(CXX) -c $(USECXXFLAGS) -o$@ -MMD -MF$(@:%.o=%.d) $<

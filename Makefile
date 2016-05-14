# Programs
RM    = rm -rf
CXX   = clang++
MKDIR = mkdir -p

# Files
SOURCE_PATH  = src
SOURCE_FILES = main.cpp common.cpp

OUTPUT_PATH = dist
OUTPUT_FILE = olec

BIN  = $(OUTPUT_PATH)/$(OUTPUT_FILE)
OBJS = $(SOURCE_FILES:%.cpp=$(OUTPUT_PATH)/%.o)
DEPS = $(SOURCE_FILES:%.cpp=$(OUTPUT_PATH)/%.d)

# Flags
USECXXFLAGS = $(CXXFLAGS) -std=c++14 -D_GLIBCXX_USE_C99 -D_XOPEN_SOURCE \
              -O0 -g -DDEBUG -fmessage-length=0 -Wall -Wextra -pedantic \
              -pthread
USELDFLAGS  = $(LDFLAGS) -pthread
USELDLIBS   = $(LDLIBS) -ltermbox -llua

# Default Targets
all: $(BIN)

gdb:
	gdb -ex run $(BIN)

clean:
	$(RM) $(OUTPUT_PATH)

# Dependencies
-include $(DEPS)

# C++ Targets
$(BIN): $(OBJS)
	$(CXX) $(USELDFLAGS) -o$@ $(OBJS) $(USELDLIBS)

$(OUTPUT_PATH)/%.o: $(SOURCE_PATH)/%.cpp
	@$(MKDIR) $(dir $@)
	$(CXX) -c $(USECXXFLAGS) -o$@ -MMD -MF$(@:%.o=%.d) $<

.PHONY: all gdb clean

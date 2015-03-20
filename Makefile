# Utilities
RM              = rm -rf
MKDIR           = mkdir -p
LINK            = ln -sf
EXEC            = exec
INSTALL         = install
DEBUGGER        = gdb -ex run
MEMCHECKER      = valgrind --leak-check=full

# Configuration
BASENAME        = olec

# Local Directories
SRCDIR          = src

# Artifacts
FILES           = main.cc terminal.cc ipc.cc app.cc keymap.cc
OBJS            = $(FILES:%.cc=$(SRCDIR)/%.o)
DEPS            = $(FILES:%.cc=$(SRCDIR)/%.d)
EXEOUTPUT       = $(BASENAME)

# On Debug
ifeq ($(DEBUG), 1)
	DEBUGCFLAGS = -DDEBUG -g
else
	DEBUGCFLAGS = -DNDEBUG
endif

# Compiler
CXX             ?= clang++
CXXFLAGS        += -std=c++11 -O2 -fmessage-length=0 \
                   -Wall -Wextra -pedantic -Wno-unused-parameter \
                   -fno-rtti \
                   -D_POSIX_SOURCE -D_GNU_SOURCE $(DEBUGCFLAGS) \
                   $(shell pkg-config --cflags gtk+-3.0 vte-2.91)
LDFLAGS         += -flto
LDLIBS          := $(shell pkg-config --libs gtk+-3.0 vte-2.91) \
                   -lncurses -levent -lv8

# Default Targets
all: $(EXEOUTPUT)

clean:
	$(RM) $(DEPS) $(OBJS)
	$(RM) $(EXEOUTPUT) $(DISTDIR)

test: $(EXEOUTPUT)
	$(EXEC) $(realpath $(EXEOUTPUT))

gdb: $(EXEOUTPUT)
	$(DEBUGGER) $(realpath $(EXEOUTPUT))

valgrind: $(EXEOUTPUT)
	$(MEMCHECKER) $(realpath $(EXEOUTPUT))

reload: all
	kill -s USR1 $$(pidof $(BASENAME) | cut -d " " -f 1)

# Targets
-include $(DEPS)

# Shared Object
$(EXEOUTPUT): $(OBJS) Makefile
	@$(MKDIR) $(dir $@)
	$(CXX) $(LDFLAGS) -o$@ $(OBJS) $(LDLIBS)

$(SRCDIR)/%.o: $(SRCDIR)/%.cc Makefile
	@$(MKDIR) $(dir $@)
	$(CXX) -c $(CXXFLAGS) -MMD -MF$(@:%.o=%.d) -MT$@ -o$@ $<

# Phony
.PHONY: all clean test

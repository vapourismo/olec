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
FILES           = main.c terminal.c event.c parent.c child.c olec.c
OBJS            = $(FILES:%.c=$(SRCDIR)/%.o)
DEPS            = $(FILES:%.c=$(SRCDIR)/%.d)
EXEOUTPUT       = $(BASENAME)

# On Debug
ifeq ($(DEBUG), 1)
	DEBUGCFLAGS = -DDEBUG -g
endif

# Compiler
CC              ?= clang
CFLAGS          += -std=c99 -O2 -fmessage-length=0 \
                   -Wall -Wextra -pedantic -Wno-unused-parameter \
                   -D_POSIX_SOURCE -D_GNU_SOURCE $(DEBUGCFLAGS) \
                   $(shell pkg-config --cflags gtk+-3.0 vte-2.91)
LDFLAGS         += -flto
LDLIBS          := $(shell pkg-config --libs gtk+-3.0 vte-2.91) -lncurses -levent

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

# Targets
-include $(DEPS)

# Shared Object
$(EXEOUTPUT): $(OBJS) Makefile
	@$(MKDIR) $(dir $@)
	$(CC) $(LDFLAGS) -o$@ $(OBJS) $(LDLIBS)

$(SRCDIR)/%.o: $(SRCDIR)/%.c Makefile
	@$(MKDIR) $(dir $@)
	$(CC) -c $(CFLAGS) -MMD -MF$(@:%.o=%.d) -MT$@ -o$@ $<

# Phony
.PHONY: all clean test

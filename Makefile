# Makefile for dasm DCPU assembler
# By Steven Smith

CC= clang
ANALYZER= clang --analyze
WARNS= -W -Wall -Werror
CFLAGS= -g $(WARNS)
LIBS=
SRC= src
BUILD= build

# Helper functions
# Turns sources into corresponding object files
getobjs= $(addprefix $(BUILD)/, $(patsubst %.c,%.o,$(1)))

# Gets artifacts that actually exist from a list of artifacts
getarts= $(wildcard $(call getobjs,$(1)) $(2))

# Assembler sources
DASM_O= dasm
DASM_S= assembler.c

DASM_ARTIFACTS= $(call getarts,$(DASM_S)) $(DASM_O)

all: $(DASM_O)

gcc: CC= gcc
gcc: ANALYZER=
gcc: $(DASM_O)

$(DASM_O): $(call getobjs, $(DASM_S))
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

$(BUILD):
	mkdir $(BUILD)

$(BUILD)/%.o: $(SRC)/%.c | $(BUILD)
	$(if $(ANALYZER), $(ANALYZER) $(WARNS) $<)
	$(CC) $(CFLAGS) -I $(SRC) -c -o $@ $<

clean:
	$(if $(DASM_ARTIFACTS), rm $(DASM_ARTIFACTS))
	$(if $(wildcard $(BUILD)), rmdir $(BUILD))

.PHONY: all clean none

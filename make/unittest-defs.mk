CCACHE :=


# Define toolchain component names.
CC      = $(CCACHE) $(TCHAIN_PREFIX)gcc
CXX     = $(CCACHE) $(TCHAIN_PREFIX)g++
AR      = $(TCHAIN_PREFIX)ar
OBJCOPY = $(TCHAIN_PREFIX)objcopy
OBJDUMP = $(TCHAIN_PREFIX)objdump
SIZE    = $(TCHAIN_PREFIX)size
NM      = $(TCHAIN_PREFIX)nm
STRIP   = $(TCHAIN_PREFIX)strip
GCOV    = $(TCHAIN_PREFIX)gcov

THUMB   = -mthumb

toprel = $(subst $(realpath $(ROOT_DIR))/,,$(abspath $(1)))



# Compile: create object files from C source files.
define COMPILE_C_TEMPLATE
$(OUTDIR)/$(notdir $(basename $(1))).o : EXTRA_FLAGS := $(2)
$(OUTDIR)/$(notdir $(basename $(1))).o : $(1)
	@echo $(MSG_COMPILING) $$(call toprel, $$<)
	$(V1) $(CC) -c $(THUMB) $$(CFLAGS) $$(CONLYFLAGS) $$(EXTRA_FLAGS) $$< -o $$@
endef

# Compile: create object files from C++ source files.
define COMPILE_CXX_TEMPLATE
$(OUTDIR)/$(notdir $(basename $(1))).o : EXTRA_FLAGS := $(2)
$(OUTDIR)/$(notdir $(basename $(1))).o : $(1)
	@echo $(MSG_COMPILINGCXX) $$(call toprel, $$<)
	$(V1) $(CXX) -c $(THUMB) $$(CFLAGS) $$(CPPFLAGS) $$(CXXFLAGS) $$(EXTRA_FLAGS) $$< -o $$@
endef

# Link: create ELF output file from object files.
#   $1 = elf file to produce
#   $2 = list of object files that make up the elf file
define LINK_CXX_TEMPLATE
.SECONDARY : $(1)
.PRECIOUS : $(2)
$(1):  $(2)
	@echo $(MSG_LINKING) $$(call toprel, $$@)
	$(V1) $(CXX) $(THUMB) $$(CFLAGS) $(2) --output $$@ $$(LDFLAGS)
endef


# Generate GCOV summary
#  $(1) = name of source file to analyze with gcov
define GCOV_TEMPLATE
$(OUTDIR)/$(1).gcov: $(OUTDIR)/$$(basename $(1)).gcda
	$(V0) @echo $(MSG_GCOV) $$(call toprel, $$@)
	$(V1) ( \
	  cd $(OUTDIR) && \
	  $(GCOV) $(1) 2>&1 > /dev/null ; \
	)
endef


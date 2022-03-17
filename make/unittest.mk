###############################################################################
# @file       unittest.mk
# @author
# @addtogroup 
# @{
# @addtogroup 
# @{
# @brief Makefile template for unit tests
###############################################################################

# Flags passed to the preprocessor.
CPPFLAGS += -I$(GTEST_DIR)/include

# Flags passed to the C++ compiler.
CXXFLAGS += -g -Wall -Wextra -Wno-missing-field-initializers -std=c++11

# Google Test needs the pthread library
LDFLAGS += -lpthread

# Google Test requires visibility of gtest includes
GTEST_CXXFLAGS := -I$(GTEST_DIR)

# gcov requires specific link options to enable the coverage hooks
LDFLAGS += -fprofile-arcs

# gcov requires specific compile options to enable the profiling hooks
GCOV_CFLAGS := -fprofile-arcs -ftest-coverage


#################################
#
# Template to build the user test
#
#################################

# Need to disable THUMB mode for unit tests
override THUMB :=

EXTRAINCDIRS    += .
UTMOCKSRC       := $(wildcard ./*.c)
ALLSRC          := $(SRC) $(UTMOCKSRC)
ALLCPPSRC       := $(wildcard ./*.cpp) $(GTEST_DIR)/src/gtest_main.cc
ALLSRCBASE      := $(notdir $(basename $(ALLSRC) $(ALLCPPSRC)))
ALLOBJ          := $(addprefix $(OUTDIR)/, $(addsuffix .o, $(ALLSRCBASE)))

# Build mock versions of APIs required to wrap the code being unit tested
$(foreach src,$(UTMOCKSRC),$(eval $(call COMPILE_C_TEMPLATE,$(src))))

# Build the code being unit tested.
# Enable gcov flags for these files so we can measure the coverage of our unit test
$(foreach src,$(SRC),$(eval $(call COMPILE_C_TEMPLATE,$(src),$(GCOV_CFLAGS))))

# Build any C++ supporting files
$(foreach src,$(ALLCPPSRC),$(eval $(call COMPILE_CXX_TEMPLATE,$(src))))

# Specific extensions to CXXFLAGS only for the google test library
$(eval $(call COMPILE_CXX_TEMPLATE, $(GTEST_DIR)/src/gtest-all.cc,$(GTEST_CXXFLAGS)))

$(eval $(call LINK_CXX_TEMPLATE,$(OUTDIR)/$(TARGET).elf,$(ALLOBJ) $(OUTDIR)/gtest-all.o))

.PHONY: elf
elf: $(OUTDIR)/$(TARGET).elf

.PHONY: xml
xml: $(OUTDIR)/$(TARGET).xml

$(OUTDIR)/$(TARGET).xml: $(OUTDIR)/$(TARGET).elf
	$(V0) @echo " TEST XML  $(MSG_EXTRA)  $(call toprel, $@)"
	$(V1) $< --gtest_output=xml:$(OUTDIR)/$(TARGET).xml > /dev/null

.PHONY: run
run: $(OUTDIR)/$(TARGET).elf
	$(V0) @echo " TEST RUN  $(MSG_EXTRA)  $(call toprel, $<)"
	$(V1) $<

GCOV_INPUT_FILES := $(notdir $(SRC))
$(foreach src,$(GCOV_INPUT_FILES),$(eval $(call GCOV_TEMPLATE,$(src))))

.PHONY: gcov
gcov: $(OUTDIR)/$(TARGET).xml
gcov: $(foreach gc,$(GCOV_INPUT_FILES),$(addsuffix .gcov, $(OUTDIR)/$(gc)))

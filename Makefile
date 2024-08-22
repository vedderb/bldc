# Makefile for Rootloader project
.DEFAULT_GOAL := help

WHEREAMI := $(dir $(lastword $(MAKEFILE_LIST)))
ROOT_DIR := $(realpath $(WHEREAMI)/ )

# Define a recursive wildcard function
# C.f. https://stackoverflow.com/a/18258352
rwildcard=$(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

# Get the raw paths for all *.h files
RAW_TARGET_PATHS := $(call rwildcard,$(ROOT_DIR)/hwconf,*.h)

# Get the target paths by filtering out any core.h files, then stripping extra whitespace
TARGET_PATHS := $(strip $(filter-out %core.h,$(RAW_TARGET_PATHS)))

# Strip the paths down to just the names. Do this by first using `notdir` to remove the paths, then the prefix (hw_), then remove the suffix (.h). Finally, sort into lexical order.
ALL_BOARD_NAMES := $(sort $(subst .h,,$(subst hw_,,$(filter hw_%, $(notdir $(TARGET_PATHS))))))

# configure some directories that are relative to wherever ROOT_DIR is located
TOOLS_DIR := $(ROOT_DIR)/tools
MAKE_DIR := $(ROOT_DIR)/make
BUILD_DIR := $(ROOT_DIR)/build
DL_DIR    := $(ROOT_DIR)/downloads

# import macros common to all supported build systems
include $(ROOT_DIR)/make/system-id.mk

# import macros that are OS specific
include $(ROOT_DIR)/make/$(OSFAMILY).mk

# include the tools makefile
include $(ROOT_DIR)/make/tools.mk

# Clean out undesirable variables from the environment and command-line
# to remove the chance that they will cause problems with our build
define SANITIZE_VAR
$(if $(filter-out undefined,$(origin $(1))),
  $(info *NOTE*      Sanitized $(2) variable '$(1)' from $(origin $(1)))
  MAKEOVERRIDES = $(filter-out $(1)=%,$(MAKEOVERRIDES))
  override $(1) :=
  unexport $(1)
)
endef

# These specific variables can influence gcc in unexpected (and undesirable) ways
SANITIZE_GCC_VARS := TMPDIR GCC_EXEC_PREFIX COMPILER_PATH LIBRARY_PATH
SANITIZE_GCC_VARS += CFLAGS CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH OBJC_INCLUDE_PATH DEPENDENCIES_OUTPUT
SANITIZE_GCC_VARS += ARCHFLAGS
$(foreach var, $(SANITIZE_GCC_VARS), $(eval $(call SANITIZE_VAR,$(var),disallowed)))

# These specific variables used to be valid but now they make no sense
SANITIZE_DEPRECATED_VARS := FOO_BAR
$(foreach var, $(SANITIZE_DEPRECATED_VARS), $(eval $(call SANITIZE_VAR,$(var),deprecated)))

# Decide on a verbosity level based on the V= parameter
export AT := @

ifndef V
export V0    :=
export V1    := $(AT)
else ifeq ($(V), 0)
export V0    := $(AT)
export V1    := $(AT)
else ifeq ($(V), 1)
endif


##############################
#
# Help instructions
#
##############################
.PHONY: help
help:
	@echo ""
	@echo "   This Makefile is known to work on Linux and Mac in a standard shell environment."
	@echo ""
	@echo "   Here is a summary of the available targets:"
	@echo ""
	@echo "   [Tool Installers]"
	@echo "     arm_sdk_install      - Install the GNU ARM gcc toolchain"
	@echo "     qt_install           - Install the all tools for Qt"
	@echo ""
	@echo "   [Big Hammer]"
	@echo "     all_fw               - Build firmware for all boards"
	@echo "     all_fw_package       - Packaage firmware for boards in package list"
	@echo ""
	@echo "   [Unit Tests]"
	@echo "     all_ut               - Build all unit tests"
	@echo "     all_ut_xml           - Run all unit tests and capture all XML output to files"
	@echo "     all_ut_run           - Run all unit tests and dump XML output to console"
	@echo ""
	@echo "   [Firmware]"
	@echo "     fw   - Build firmware for default target"
	@echo "                            supported boards are: $(ALL_BOARD_NAMES)"
	@echo "     fw_<board>   - Build firmware for target <board>"
	@echo "     PROJECT=<target> fw   - Build firmware for <target>"
	@echo "     fw_<board>_clean     - Remove firmware for <board>"
	@echo "     fw_<board>_flash     - Use OpenOCD + SWD/JTAG to write firmware to <target>"
	@echo ""
	@echo "   Hint: Add V=1 to your command line to see verbose build output."
	@echo ""
	@echo "   Note: All tools will be installed into $(TOOLS_DIR)"
	@echo "         All build output will be placed in $(BUILD_DIR)"
	@echo ""


$(DL_DIR):
	$(V1) $(MKDIR) $@

$(TOOLS_DIR):
	$(V1) $(MKDIR) $@

##############################
#
# Build and Upload
#
##############################

# $(1) = Canonical board name all in lower case (e.g. 100_250)
# $(2) = Target hardware directory
define FIND_TARGET_C_CODE
   # Remove `_no_limits`
   $(eval ROOT_TARGET_NAME = $(subst _no_limits,,$(1)))

   # Look for `*_core.c` file
   ifneq ("$(wildcard $(2)/hw_*_core.c)","")
      # Good luck, there it is!
      HW_SRC_FILE = $(wildcard $(2)/hw_*_core.c)
   else
      # There isn't one, so let's hope for the sister `.c` file
      HW_SRC_FILE = $(2)/hw_$(ROOT_TARGET_NAME).c
   endif

endef

# $(1) = Canonical board name all in lower case (e.g. 100_250)
# $(2) = firmware build directory
# $(3) = firmware name
# $(4) = git branch name
# $(5) = git hash (and dirty flag)
# $(6) = compiler version
define FW_TEMPLATE
.PHONY: $(1) fw_$(1)
$(1): fw_$(1)_vescfw
fw_$(1): fw_$(1)_vescfw

fw_$(1)_vescfw: $(eval HW_DIR = $(dir $(filter %/hw_$(1).h, $(TARGET_PATHS))))  # Find the directory for this header file
fw_$(1)_vescfw: $(eval HW_SRC_FILE = $(call FIND_TARGET_C_CODE,$(1),$(HW_DIR)))  # Find the c code associated to this header file
fw_$(1)_vescfw:
	@echo "********* BUILD: $(1) **********"
	$(V1) $(MKDIR) $(BUILD_DIR)/$(1)
	$(V1) $$(MAKE) -f $(MAKE_DIR)/fw.mk \
		TCHAIN_PREFIX="$(ARM_SDK_PREFIX)" \
		BUILDDIR="$(2)" \
		PROJECT="$(3)" \
		build_args='-DHW_SOURCE=\"$(HW_SRC_FILE)\" -DHW_HEADER=\"$(HW_DIR)/hw_$(1).h\" -DGIT_BRANCH_NAME=\"$(4)\" -DGIT_COMMIT_HASH=\"$(5)\" -DARM_GCC_VERSION=\"$(6)\"' USE_VERBOSE_COMPILE=no

$(1)_flash: fw_$(1)_flash
fw_$(1)_flash: fw_$(1)_vescfw fw_$(1)_flash_only

$(1)_flash_only: fw_$(1)_flash_only
fw_$(1)_flash_only:
	@echo "********* PROGRAM: $(1) **********"
	$(V1) openocd -f board/stm32f4discovery.cfg -c "reset_config trst_only combined" -c "program $(2)/$(3).elf verify reset exit"

.PHONY: $(1)_clean
$(1)_clean: fw_$(1)_clean
fw_$(1)_clean: TARGET=fw_$(1)
fw_$(1)_clean: OUTDIR=$(BUILD_DIR)/$$(TARGET)
fw_$(1)_clean:
	$(V0) @echo " CLEAN      $$@"
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(BUILD_DIR)/$(1)" ] || $(RM) -r "$(BUILD_DIR)/$(1)"
	$(V1) [ ! -d "$(ROOT_DIR)/.dep" ] || $(RM) -r "$(ROOT_DIR)/.dep"
else
	$(V1) powershell -noprofile -command "& {if (Test-Path $(BUILD_DIR)/$(1)) {Remove-Item -Recurse $(BUILD_DIR)/$(1)}}"
	$(V1) powershell -noprofile -command "& {if (Test-Path $(ROOT_DIR)/.dep) {Remove-Item -Recurse $(ROOT_DIR)/.dep}}"
endif
endef

clear_option_bytes:
	$(V1) openocd -f board/stm32f4discovery.cfg -c "init" -c "stm32f2x unlock 0" -c "mww 0x40023C08 0x08192A3B; mww 0x40023C08 0x4C5D6E7F; mww 0x40023C14 0x0fffaaed" -c "exit"

#program with olimex arm-usb-tiny-h and jtag-swd adapter board. needs openocd>=0.9
upload-olimex: fw
	$(V1) openocd -f interface/ftdi/olimex-arm-usb-tiny-h.cfg -f interface/ftdi/olimex-arm-jtag-swd.cfg -c "set WORKAREASIZE 0x2000" -f target/stm32f4x.cfg -c "program build/$(PROJECT).elf verify reset"

upload-pi: fw
	$(V1) openocd -f pi_stm32.cfg -c "reset_config trst_only combined" -c "program build/$(PROJECT).elf verify reset exit"

upload-pi-remote: fw
	$(V1) ./upload_remote_pi build/$(PROJECT).elf ted 10.42.0.199 22

debug-start:
	$(V1) openocd -f stm32-bv_openocd.cfg

size: build/$(PROJECT).elf
	@$(SZ) $<

# Generate the targets for whatever boards are in each list
FW_TARGETS := $(addprefix fw_, $(ALL_BOARD_NAMES))

.PHONY: all_fw all_fw_clean
all_fw:        $(addsuffix _vescfw, $(FW_TARGETS))
all_fw_clean:  $(addsuffix _clean,  $(FW_TARGETS))

# Expand the firmware rules
$(foreach board, $(ALL_BOARD_NAMES), $(eval $(call FW_TEMPLATE,$(board),$(BUILD_DIR)/$(board),$(board),$(GIT_BRANCH_NAME),$(GIT_COMMIT_HASH)$(GIT_DIRTY_LABEL),$(ARM_GCC_VERSION))))


##############################
#
# Packaging
#
##############################

.PHONY: all_fw_package
all_fw_package: all_fw all_fw_package_clean
	$(V0) @echo " PACKAGE        $(ROOT_DIR)/package/*"

# Place all firmware files into `./package` directory
	$(V1) $(PYTHON) package_firmware.py

# Find all the leftover object and lst files
	$(eval BUILD_CRUFT := $(call rwildcard,$(ROOT_DIR)/build,*.lst *.o))

# Delete the cruft files, so as not to unnecessarily consume GB of space
ifneq ($(OSFAMILY), windows)
	$(V1) $(RM) $(BUILD_CRUFT)
else
	$(V1) powershell -noprofile -command "& {Remove-Item $(BUILD_CRUFT)}"
endif

.PHONY: all_fw_package_clean
all_fw_package_clean:
	$(V0) @echo " CLEAN        $(ROOT_DIR)/package/*"
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(ROOT_DIR)/package/" ] || $(RM) -rf $(ROOT_DIR)/package/*
else
	$(V1) powershell -noprofile -command "& {if (Test-Path $(ROOT_DIR)/package/*) {Remove-Item -Recurse $(ROOT_DIR)/package/*}}"
endif


##############################
#
# Unit Tests
#
##############################

ALL_UNITTESTS := utils_math

UT_OUT_DIR := $(BUILD_DIR)/unit_tests

$(UT_OUT_DIR):
	$(V1) $(MKDIR) $@

.PHONY: all_ut
all_ut: $(addsuffix _elf, $(addprefix ut_, $(ALL_UNITTESTS))) $(ALL_PYTHON_UNITTESTS)

.PHONY: all_ut_xml
all_ut_xml: $(addsuffix _xml, $(addprefix ut_, $(ALL_UNITTESTS)))

.PHONY: all_ut_run
all_ut_run: $(addsuffix _run, $(addprefix ut_, $(ALL_UNITTESTS))) $(ALL_PYTHON_UNITTESTS)

.PHONY: all_ut_gcov
all_ut_gcov: | $(addsuffix _gcov, $(addprefix ut_, $(ALL_UNITTESTS)))

.PHONY: all_ut_clean
all_ut_clean:
	$(V0) @echo " CLEAN      $@"
	$(V1) [ ! -d "$(UT_OUT_DIR)" ] || $(RM) -r "$(UT_OUT_DIR)"

# $(1) = Unit test name
define UT_TEMPLATE
.PHONY: ut_$(1)
ut_$(1): ut_$(1)_run
ut_$(1)_gcov: | ut_$(1)_xml

ut_$(1)_%: TARGET=$(1)
ut_$(1)_%: OUTDIR=$(UT_OUT_DIR)/$$(TARGET)
ut_$(1)_%: UT_ROOT_DIR=$(ROOT_DIR)/tests/$(1)
ut_$(1)_%: $$(UT_OUT_DIR)
	$(V1) $(MKDIR) $(UT_OUT_DIR)/$(1)
	$(V1) cd $$(UT_ROOT_DIR) && \
		$$(MAKE) -r --no-print-directory \
		BUILD_TYPE=ut \
		TCHAIN_PREFIX="" \
		REMOVE_CMD="$(RM)" \
		\
		MAKE_INC_DIR=$(MAKE_INC_DIR) \
		ROOT_DIR=$(ROOT_DIR) \
		TARGET=$$(TARGET) \
		OUTDIR=$$(OUTDIR) \
		\
		GTEST_DIR=$(GTEST_DIR) \
		\
		$$*

.PHONY: ut_$(1)_clean
ut_$(1)_clean: TARGET=$(1)
ut_$(1)_clean: OUTDIR=$(UT_OUT_DIR)/$$(TARGET)
ut_$(1)_clean:
	$(V0) @echo " CLEAN      $(1)"
	$(V1) [ ! -d "$$(OUTDIR)" ] || $(RM) -r "$$(OUTDIR)"
endef

# Expand the unittest rules
$(foreach ut, $(ALL_UNITTESTS), $(eval $(call UT_TEMPLATE,$(ut))))

# Disable parallel make when the all_ut_run target is requested otherwise the TAP/XML
# output is interleaved with the rest of the make output.
ifneq ($(strip $(filter all_ut_run,$(MAKECMDGOALS))),)
.NOTPARALLEL:
$(info *NOTE*     Parallel make disabled by all_ut_run target so we have sane console output)
endif

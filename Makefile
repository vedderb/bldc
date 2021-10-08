# Makefile for Rootloader project
.DEFAULT_GOAL := help

# Define build target, if it is not already defined on the command line
export PROJECT ?= BLDC_4_ChibiOS

WHEREAMI := $(dir $(lastword $(MAKEFILE_LIST)))
ROOT_DIR := $(realpath $(WHEREAMI)/ )

# import macros common to all supported build systems
include $(ROOT_DIR)/make/system-id.mk

# configure some directories that are relative to wherever ROOT_DIR is located
TOOLS_DIR := $(ROOT_DIR)/tools
BUILD_DIR := $(ROOT_DIR)/build
DL_DIR    := $(ROOT_DIR)/downloads

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
	@echo
	@echo "   This Makefile is known to work on Linux and Mac in a standard shell environment."
	@echo
	@echo "   Here is a summary of the available targets:"
	@echo
	@echo "   [Tool Installers]"
	@echo "     arm_sdk_install      - Install the GNU ARM gcc toolchain"
	@echo
	@echo
	@echo "   [Firmware]"
	@echo "     fw   - Build firmware for default target"
	@echo "     PROJECT=<target> fw   - Build firmware for <target>"
	@echo
	@echo "   Hint: Add V=1 to your command line to see verbose build output."
	@echo
	@echo "   Note: All tools will be installed into $(TOOLS_DIR)"
	@echo "         All build output will be placed in $(BUILD_DIR)"
	@echo


$(DL_DIR):
	mkdir -p $@

$(TOOLS_DIR):
	mkdir -p $@

##############################
#
# Build and Upload
#
##############################
.PHONY: fw
fw:
	@echo "*********BUILD**********"
	$(V1) make -f make/fw.mk \
		TCHAIN_PREFIX="$(ARM_SDK_PREFIX)" \
		PROJECT="$(PROJECT)" \

upload: fw upload_only

upload_only:
	$(V1) openocd -f board/stm32f4discovery.cfg -c "reset_config trst_only combined" -c "program build/$(PROJECT).elf verify reset exit"

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

clean: 
	$(V0) @echo " CLEAN      $$@"
	$(V1) [ ! -d "$(BUILD_DIR)" ] || $(RM) -r "$(BUILD_DIR)"

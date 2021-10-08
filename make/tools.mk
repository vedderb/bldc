###############################################################
#
# Installers for tools required by the rootloader builds
#
# NOTE: These are not tied to the default goals
#       and must be invoked manually
#
###############################################################


####################
# ARM (Cortex) SDK #
####################
ARM_SDK_DIR := $(TOOLS_DIR)/gcc-arm-none-eabi-7-2018-q2-update

.PHONY: arm_sdk_install
ifdef LINUX
  arm_sdk_install: ARM_SDK_URL  := https://developer.arm.com/-/media/Files/downloads/gnu-rm/7-2018q2/gcc-arm-none-eabi-7-2018-q2-update-linux.tar.bz2
endif

ifdef MACOS
  arm_sdk_install: ARM_SDK_URL  := https://developer.arm.com/-/media/Files/downloads/gnu-rm/7-2018q2/gcc-arm-none-eabi-7-2018-q2-update-mac.tar.bz2
endif

ifdef WINDOWS
  arm_sdk_install: ARM_SDK_URL  := https://developer.arm.com/-/media/Files/downloads/gnu-rm/7-2018q2/gcc-arm-none-eabi-7-2018-q2-update-win32.zip
endif

arm_sdk_install: ARM_SDK_FILE := $(notdir $(ARM_SDK_URL))
# order-only prereq on directory existance:
arm_sdk_install: | $(DL_DIR) $(TOOLS_DIR)
arm_sdk_install: arm_sdk_clean
ifneq ($(OSFAMILY), windows)
	# download the source only if it's newer than what we already have
	$(V1) wget --no-check-certificate -N -P "$(DL_DIR)" "$(ARM_SDK_URL)"

	# binary only release so just extract it
	$(V1) tar -C $(TOOLS_DIR) -xjf "$(DL_DIR)/$(ARM_SDK_FILE)"
else
	$(V1) curl -L -k -o "$(DL_DIR)/$(ARM_SDK_FILE)" "$(ARM_SDK_URL)"
	$(V1) unzip -q -d $(ARM_SDK_DIR) "$(DL_DIR)/$(ARM_SDK_FILE)"
endif

.PHONY: arm_sdk_clean
arm_sdk_clean:
	$(V1) [ ! -d "$(ARM_SDK_DIR)" ] || $(RM) -r $(ARM_SDK_DIR)


##############################
#
# Set up paths to tools
#
##############################

ifeq ($(shell [ -d "$(ARM_SDK_DIR)" ] && echo "exists"), exists)
  ARM_SDK_PREFIX := $(ARM_SDK_DIR)/bin/arm-none-eabi-
else
  ifneq ($(MAKECMDGOALS),arm_sdk_install)
    $(info **WARNING** ARM-SDK not in $(ARM_SDK_DIR)  Please run 'make arm_sdk_install')
  endif
  # not installed, hope it's in the path...
  ARM_SDK_PREFIX ?= arm-none-eabi-
endif

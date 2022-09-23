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
	$(V1) curl --continue - --location --insecure --output "$(DL_DIR)/$(ARM_SDK_FILE)" "$(ARM_SDK_URL)"
	$(V1) powershell -noprofile -command Expand-Archive -DestinationPath $(ARM_SDK_DIR) -LiteralPath "$(DL_DIR)/$(ARM_SDK_FILE)"

endif

.PHONY: arm_sdk_clean
arm_sdk_clean:
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(ARM_SDK_DIR)" ] || $(RM) -r $(ARM_SDK_DIR)
else
	$(V1) powershell -noprofile -command "& {if (Test-Path $(ARM_SDK_DIR)) {Remove-Item -Recurse $(ARM_SDK_DIR)}}"
endif


##########
# Qt SDK #
##########
QT_ROOT := $(TOOLS_DIR)/Qt
QT_SDK_VER := 5.15.2
QT_SDK_DIR := $(QT_ROOT)/$(QT_SDK_VER)

ifdef LINUX
  QT_SDK_ARCH  := gcc_64
  QT_SDK_HOST  := linux
  QT_BIN       := $(QT_SDK_DIR)/$(QT_SDK_ARCH)/bin
  QT_SDKTOOL   := $(QT_ROOT)/Tools/QtCreator/libexec/qtcreator/sdktool
  QMAKE        := qmake
endif

ifdef MACOS
  QT_SDK_ARCH  := clang_64
  QT_SDK_HOST  := mac
  QT_BIN       := $(QT_SDK_DIR)/$(QT_SDK_ARCH)/bin
  QT_SDKTOOL  := $(TOOLS_DIR)/Qt/Qt\ Creator.app/Contents/Resources/libexec/sdktool
  QMAKE       := qmake
endif

ifdef WINDOWS
  QT_SDK_ARCH  := win64_msvc2019_64
  QT_SDK_HOST  := windows
  QT_BIN       := $(QT_SDK_DIR)/msvc2019_64/bin
  QT_SDKTOOL  := $(QT_ROOT)\Tools\QtCreator\bin\sdktool.exe
  QMAKE       := qmake.exe
endif

.PHONY: qt_sdk_install

qt_sdk_install: QT_SDK_FILE := $(notdir $(QT_SDK_URL))
# order-only prereq on directory existance:
qt_sdk_install: | $(DL_DIR) $(TOOLS_DIR)
qt_sdk_install: qt_sdk_clean
# binary only release so just download and extract it
	$(V1) $(PYTHON) -m aqt install-qt --keep --archive-dest "$(DL_DIR)/Qt" $(QT_SDK_HOST) desktop $(QT_SDK_VER) $(QT_SDK_ARCH) --outputdir $(QT_ROOT)

.PHONY: qt_sdk_clean
qt_sdk_clean:
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(QT_SDK_DIR)" ] || $(RM) -r $(QT_SDK_DIR)
else
	$(V1) powershell -noprofile -command "& {if (Test-Path $(QT_SDK_DIR)) {Remove-Item -Recurse $(QT_SDK_DIR)}}"
endif


#################
# linuxdeployqt #
#################

.PHONY: qt_linuxdeployqt_install
ifdef LINUX
qt_linuxdeployqt_install: LINUXDEPLOYQT_URL  := https://github.com/probonopd/linuxdeployqt/releases/download/continuous/linuxdeployqt-continuous-x86_64.AppImage
qt_linuxdeployqt_install: LINUXDEPLOYQT_FILE := linuxdeployqt-continuous-x86_64.AppImage
qt_linuxdeployqt_install:
	# download the source only if it's newer than what we already have
	$(V1) wget --no-check-certificate -N -P "$(DL_DIR)" "$(LINUXDEPLOYQT_URL)"

	# Set the file to executable
	$(V1) chmod +x "$(DL_DIR)/$(LINUXDEPLOYQT_FILE)"

	# Move the file
	$(V1) mv "$(DL_DIR)/$(LINUXDEPLOYQT_FILE)" "$(TOOLS_DIR)/Qt"
endif



##############
# Qt Creator #
##############
QT_CREATOR_DIR := $(TOOLS_DIR)/Qt

.PHONY: qt_creator_install
ifdef LINUX
  qt_creator_install: QT_CREATOR_HOST  := linux
endif

ifdef MACOS
  qt_creator_install: QT_CREATOR_HOST  := mac
endif

ifdef WINDOWS
  qt_creator_install: QT_CREATOR_HOST  := windows
endif

qt_creator_install:
# binary only release so just download and extract it
	$(V1) $(PYTHON) -m aqt install-tool --keep --archive-dest "$(DL_DIR)/Qt" $(QT_CREATOR_HOST) desktop tools_qtcreator qt.tools.qtcreator --outputdir $(QT_CREATOR_DIR)

.PHONY: qt_creator_configure
qt_creator_configure:
# Create a shared Qt project file with all the targets
	$(V1) $(PYTHON) Project/scripts/qt_creator_firmware_configuration.py --targets $(ALL_BOARD_NAMES) sim_posix
	$(V1) $(QT_SDKTOOL) addQt \
	  --id qt.vesc \
	  --name "VESC Firmware" \
	  --qmake $(QT_BIN)/$(QMAKE) \
	  --type Qt4ProjectManager.QtVersion.Desktop
	$(V1) $(QT_SDKTOOL) addKit \
	    --id qt.vesc \
	    --name "VESC Firmware" \
	    --devicetype Desktop \
	    --qt qt.vesc

.PHONY: qt_creator_uninstall
qt_creator_uninstall:
	$(V1) $(QT_SDKTOOL) rmQt --id qt.vesc
	$(V1) $(QT_SDKTOOL) rmKit --id qt.vesc

.PHONY: qt_creator_clean
qt_creator_clean: qt_creator_uninstall
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(QT_CREATOR_DIR)" ] || $(RM) -r $(QT_CREATOR_DIR)
else
	$(V1) powershell -noprofile -command "& {if (Test-Path $(QT_CREATOR_DIR)) {Remove-Item -Recurse $(QT_CREATOR_DIR)}}"
endif


##########
# All Qt #
##########


.PHONY: qt_install
qt_install: qt_sdk_install qt_creator_install qt_creator_configure qt_linuxdeployqt_install

###############
# Google Test #
###############

# Set up Google Test (gtest) tools
GTEST_DIR       := $(TOOLS_DIR)/gtest-1.11.0

.PHONY: gtest_install
gtest_install: | $(DL_DIR) $(TOOLS_DIR)
gtest_install: GTEST_URL  := https://github.com/google/googletest/archive/refs/tags/release-1.11.0.zip
gtest_install: GTEST_FILE := $(notdir $(GTEST_URL))
gtest_install: gtest_clean
ifneq ($(OSFAMILY), windows)
	# download the file unconditionally since google code gives back 404
	# for HTTP HEAD requests which are used when using the wget -N option
	$(V1) [ ! -f "$(DL_DIR)/$(GTEST_FILE)" ] || $(RM) -f "$(DL_DIR)/$(GTEST_FILE)"
	$(V1) wget -P "$(DL_DIR)" "$(GTEST_URL)"

	# extract the source
	$(V1) [ ! -d "$(GTEST_DIR)" ] || $(RM) -rf "$(GTEST_DIR)"
	$(V1) $(MKDIR) "$(GTEST_DIR)"
	$(V1) unzip -q -d "$(TOOLS_DIR)" "$(DL_DIR)/$(GTEST_FILE)"
else
	$(V1) curl --continue - --location --insecure --output "$(DL_DIR)/$(GTEST_FILE)" "$(GTEST_URL)"
	$(V1) powershell -noprofile -command Expand-Archive -DestinationPath $(GTEST_DIR) -LiteralPath "$(DL_DIR)/$(GTEST_FILE)"
endif

.PHONY: gtest_clean
gtest_clean:
	$(V0) @echo " CLEAN        $(GTEST_DIR)"
ifneq ($(OSFAMILY), windows)
	$(V1) [ ! -d "$(GTEST_DIR)" ] || $(RM) -rf "$(GTEST_DIR)"
else
	$(V1) powershell -noprofile -command if (Test-Path $(GTEST_DIR)) {Remove-Item -Recurse $(GTEST_DIR)}
endif


##############################
#
# Set up paths to tools
#
##############################

ifneq ("$(wildcard $(ARM_SDK_DIR))","")
  ARM_SDK_PREFIX := $(ARM_SDK_DIR)/bin/arm-none-eabi-
else
  ifneq ($(MAKECMDGOALS),arm_sdk_install)
    $(info **WARNING** ARM-SDK not in $(ARM_SDK_DIR)  Please run 'make arm_sdk_install')
  endif
  # not installed, hope it's in the path...
  ARM_SDK_PREFIX ?= arm-none-eabi-
endif


# Get the ARM GCC version
ifneq ("$(ARM_SDK_PREFIX)","")
  ARM_GCC_VERSION := $(shell $(ARM_SDK_PREFIX)gcc -dumpversion)
endif

# Get the git branch name, commit hash, and clean/dirty state
GIT_BRANCH_NAME := $(shell git rev-parse --abbrev-ref HEAD)
GIT_COMMIT_HASH := $(shell git rev-parse --short HEAD)
GIT_DIRTY_LABEL := $(shell git diff --quiet || echo -dirty)


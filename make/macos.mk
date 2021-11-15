# macosx.mk
#
# Goals:
#   Configure an environment that will allow VESC's code to be built on a macOS
#   system. The environment will support the current versions of python installed
#   either via brew or system default

############################
# Check for and find Python
############################

ifneq (, $(shell which /usr/local/bin/python))
  # Homebrew python present. Use it.
  PYTHON_ROOT:=/usr/local/bin/
else
  # Homebrew python not present. Use the system python command.
  PYTHON_ROOT:=/usr/bin/
endif

#include $(ROOT_DIR)/make/python.mk

############################
# Configure shell commands
############################

RM := rm
MKDIR := mkdir -p

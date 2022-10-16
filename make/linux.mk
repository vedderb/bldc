# linux.mk
#
# Goals:
#   Configure an environment that will allow VESC's code to be built on a Linux
#   system.


############################
# Check for and find Python
############################
ifneq ($(shell which python3), "")
	PYTHON := python3
else ifneq ($(shell which python), "")
	PYTHON := python
else
	$(warning WARN no python found)
endif

export PYTHON

############################
# Configure shell commands
############################

RM := rm
MKDIR := mkdir -p

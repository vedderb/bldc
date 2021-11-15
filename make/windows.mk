# windows.mk
#
# Goals:
#   Configure an environment that will allow VESC's code to be built on a Windows
#   system.

############################
# Configure shell commands
############################

RM := del
MKDIR := pwsh -noprofile -command New-Item -Force -itemtype "directory"

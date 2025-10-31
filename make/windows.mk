# windows.mk
#
# Goals:
#   Configure an environment that will allow VESC's code to be built on a Windows
#   system.


############################
# Check for and find Python
############################


# Get Python version, separate major/minor/patch, then put into wordlist
PYTHON_VERSION_=$(wordlist 2,4,$(subst ., ,$(shell python -V 2>&1)))

# Get major version from aforementioned list
PYTHON_MAJOR_VERSION_=$(word 1,$(PYTHON_VERSION_))

# Just in case Make has some weird scope stuff
PYTHON=0

# If the major Python version is the one we want..
ifeq ($(PYTHON_MAJOR_VERSION_),3)
	# Then we can just use the normal Python executable
	PYTHON:=python
else
	# However, this isn't always the case..
	# Let's look for `python3`. If `where` doesn't return a null value, then
	#  it exists!
	ifneq ($(shell where python3), "")
		PYTHON:=python3
	else
		# And if it doesn't exist, let's use the default Python, and warn the user.
		# PYTHON NOT FOUND.
		PYTHON:=python \
		$(warning "Python3 not found.")
	endif
endif

export PYTHON


############################
# Configure shell commands
############################

# Set the shell to be cmd.exe
SHELL = cmd
# Override the SEHLL variable for any sub-make calls
MAKEOVERRIDES += SHELL=cmd

RM := del
MKDIR := powershell -noprofile -command New-Item -Force -itemtype "directory"

#
# Unit Tests Makefile
#
# This Makefile exports two targets to be used for unit testing.
#
#   tests    - compile unit tests.
#   runtests - execute unit tests.
#
# The tests target produces a binary: build/tests/tests. This can be invoked
# directly to pass arguments to the gTest unit testing framework.
#

# Unit Tests ###################################################################

# Add unit test source files to TEST_SRCS and include any mk files here.
include tests/util/util.mk

# gTest Directories ############################################################

BUILD_DIR = build
GTEST_BUILD_DIR = $(BUILD_DIR)/tests
GTEST_DIR = googletest/googletest

# gTest Compiler ###############################################################

GTEST_CC = g++

# gTest Linker #################################################################

GTEST_LD = g++

# gTest Compiler Flags #########################################################

GTEST_CFLAGS  = -isystem $(GTEST_DIR)/include
GTEST_CFLAGS += -I$(GTEST_DIR)
GTEST_CFLAGS += -std=c++11
GTEST_CFLAGS += -I.

# gTest Linker Flags ###########################################################

GTEST_LDFLAGS = -lpthread

# gTest Sources ################################################################

GTEST_SRCS = $(GTEST_DIR)/src/gtest-all.cc

# Test Sources #################################################################

TEST_BIN = $(BUILD_DIR)/tests/tests

TEST_SRCS += tests/tests_main.cpp

# gTest Build Rules ############################################################

GTEST_OBJS = $(patsubst %.cc, %.o, $(GTEST_SRCS))
GTEST_BUILD_OBJS = $(addprefix $(GTEST_BUILD_DIR)/, $(GTEST_OBJS))
GTEST_BUILD_DIRS = $(sort $(dir $(GTEST_BUILD_OBJS)))

libgtest : $(GTEST_BUILD_DIRS) $(GTEST_BUILD_OBJS)

$(GTEST_BUILD_DIRS):
	mkdir -p $@

$(GTEST_BUILD_OBJS) : $(GTEST_BUILD_DIR)/%.o : %.cc
	$(GTEST_CC) $(GTEST_CFLAGS) -c $< -o $@

# Unit Test Build Rules ########################################################

TEST_OBJS = $(patsubst %.cpp, %.o, $(TEST_SRCS))
TEST_BUILD_OBJS = $(addprefix $(BUILD_DIR)/, $(TEST_OBJS))
TEST_BUILD_DIRS = $(sort $(dir $(TEST_BUILD_OBJS)))

runtests : tests
	./$(TEST_BIN)

tests : $(TEST_BIN)
	
$(TEST_BIN) : libgtest $(TEST_BUILD_DIRS) $(TEST_BUILD_OBJS)
	$(GTEST_LD) $(TEST_BUILD_OBJS) $(GTEST_BUILD_OBJS) -o $@

$(TEST_BUILD_DIRS):
	mkdir -p $@

$(TEST_BUILD_OBJS) : $(BUILD_DIR)/%.o : %.cpp
	$(GTEST_CC) $(GTEST_CFLAGS) -c $< -o $@

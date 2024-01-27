first_rule: all

LISPBM_SRC = $(LISPBM)/src/env.c \
             $(LISPBM)/src/fundamental.c \
             $(LISPBM)/src/heap.c \
             $(LISPBM)/src/lbm_memory.c \
             $(LISPBM)/src/print.c \
             $(LISPBM)/src/stack.c \
             $(LISPBM)/src/symrepr.c \
             $(LISPBM)/src/tokpar.c \
             $(LISPBM)/src/extensions.c \
             $(LISPBM)/src/lispbm.c \
             $(LISPBM)/src/eval_cps.c \
             $(LISPBM)/src/lbm_c_interop.c \
             $(LISPBM)/src/lbm_custom_type.c \
             $(LISPBM)/src/lbm_channel.c \
             $(LISPBM)/src/lbm_flat_value.c\
             $(LISPBM)/src/lbm_flags.c\
             $(LISPBM)/src/lbm_prof.c\
             $(LISPBM)/src/extensions/array_extensions.c \
             $(LISPBM)/src/extensions/string_extensions.c \
             $(LISPBM)/src/extensions/math_extensions.c \
             $(LISPBM)/src/extensions/runtime_extensions.c \
             $(LISPBM)/src/extensions/matvec_extensions.c \
             $(LISPBM)/src/extensions/random_extensions.c \
             $(LISPBM)/src/extensions/loop_extensions.c


LISPBM_INC = -I$(LISPBM)/include \
             -I$(LISPBM)/include/extensions \
             -I$(LISPBM)/src

LISPBM_FLAGS = -lm
LISPBM_DEPS  =

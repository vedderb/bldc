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
             $(LISPBM)/src/lbm_defrag_mem.c\
             $(LISPBM)/src/buffer.c \
             $(LISPBM)/src/extensions/array_extensions.c \
             $(LISPBM)/src/extensions/string_extensions.c \
             $(LISPBM)/src/extensions/math_extensions.c \
             $(LISPBM)/src/extensions/runtime_extensions.c \
             $(LISPBM)/src/extensions/random_extensions.c \
	     $(LISPBM)/src/extensions/set_extensions.c \
             $(LISPBM)/src/extensions/display_extensions.c \
             $(LISPBM)/src/extensions/tjpgd.c \
             $(LISPBM)/src/extensions/mutex_extensions.c \
             $(LISPBM)/src/extensions/lbm_dyn_lib.c \
             $(LISPBM)/src/extensions/schrift.c \
             $(LISPBM)/src/extensions/ttf_extensions.c

LISPBM_H = $(LISPBM)/include/env.h \
           $(LISPBM)/include/eval_cps.h \
           $(LISPBM)/include/extensions.h \
           $(LISPBM)/include/fundamental.h \
           $(LISPBM)/include/heap.h \
           $(LISPBM)/include/heap_vis.h \
           $(LISPBM)/include/lbm_channel.h \
           $(LISPBM)/include/lbm_c_interop.h \
           $(LISPBM)/include/lbm_constants.h \
           $(LISPBM)/include/lbm_custom_type.h \
           $(LISPBM)/include/lbm_defines.h \
           $(LISPBM)/include/lbm_defrag_mem.h \
           $(LISPBM)/include/lbm_flags.h \
           $(LISPBM)/include/lbm_flat_value.h \
           $(LISPBM)/include/lbm_llama_ascii.h \
           $(LISPBM)/include/lbm_memory.h \
           $(LISPBM)/include/lbm_prof.h \
           $(LISPBM)/include/lbm_types.h \
           $(LISPBM)/include/lbm_utils.h \
           $(LISPBM)/include/lbm_version.h \
           $(LISPBM)/include/lispbm.h \
           $(LISPBM)/include/print.h \
           $(LISPBM)/include/stack.h \
           $(LISPBM)/include/symrepr.h \
           $(LISPBM)/include/tokpar.h \
           $(LISPBM)/include/buffer.h \
           $(LISPBM)/include/extensions/array_extensions.h \
           $(LISPBM)/include/extensions/display_extensions.h \
           $(LISPBM)/include/extensions/lbm_dyn_lib.h \
           $(LISPBM)/include/extensions/math_extensions.h \
           $(LISPBM)/include/extensions/random_extensions.h \
           $(LISPBM)/include/extensions/runtime_extensions.h \
           $(LISPBM)/include/extensions/set_extensions.h \
           $(LISPBM)/include/extensions/string_extensions.h \
           $(LISPBM)/include/extensions/ttf_extensions.h


LISPBM_INC = -I$(LISPBM)/include \
             -I$(LISPBM)/include/extensions \
             -I$(LISPBM)/src

LISPBM_FLAGS = -lm
LISPBM_DEPS  =

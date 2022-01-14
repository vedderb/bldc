LISPBM = lispBM

LISPBMSRC = $(LISPBM)/src/env.c \
            $(LISPBM)/src/fundamental.c \
	        $(LISPBM)/src/heap.c \
            $(LISPBM)/src/lispbm_memory.c \
            $(LISPBM)/src/print.c \
            $(LISPBM)/src/qq_expand.c \
            $(LISPBM)/src/stack.c \
            $(LISPBM)/src/symrepr.c \
            $(LISPBM)/src/tokpar.c \
            $(LISPBM)/src/compression.c \
            $(LISPBM)/src/extensions.c \
            $(LISPBM)/src/lispbm.c \
            $(LISPBM)/src/eval_cps.c \
            $(LISPBM)/platform/chibios/src/platform_mutex.c \
			$(LISPBM)/lispif.c \
			$(LISPBM)/lispif_vesc_extensions.c

LISPBMINC = lispBM \
            lispBM/include \
            lispBM/platform/chibios/include

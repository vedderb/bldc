LISPBM = lispBM/lispBM

LISPBMSRC = $(LISPBM)/src/env.c \
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
            $(LISPBM)/platform/chibios/src/platform_mutex.c \
            $(LISPBM)/src/lbm_channel.c \
            $(LISPBM)/src/lbm_c_interop.c \
            $(LISPBM)/src/lbm_custom_type.c \
            $(LISPBM)/src/lbm_flags.c \
            $(LISPBM)/src/lbm_flat_value.c \
            $(LISPBM)/src/lbm_prof.c \
            $(LISPBM)/src/extensions/array_extensions.c \
            $(LISPBM)/src/extensions/math_extensions.c \
            $(LISPBM)/src/extensions/string_extensions.c \
			lispBM/lispif.c \
			lispBM/lispif_vesc_extensions.c \
			lispBM/lispif_vesc_dynamic_loader.c \
			lispBM/lispif_c_lib.c \
            lispBM/lbm_vesc_utils.c

LISPBMINC = lispBM \
			$(LISPBM) \
            $(LISPBM)/include \
            $(LISPBM)/platform/chibios/include

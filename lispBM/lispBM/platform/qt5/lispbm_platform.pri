
LISPBM_ROOT = $$PWD/../..

INCLUDEPATH += \
    $$LISPBM_ROOT/include \
    $$LISPBM_ROOT/include/extensions \
    $$LISPBM_ROOT/src \
    $$LISPBM_ROOT/utils \
    $$PWD/include

!contains(DEFINES, LBM32): DEFINES += LBM64

DEFINES += \
    FULL_RTS_LIB \
    LBM_USE_DYN_MACROS \
    LBM_USE_DYN_LOOPS \
    LBM_USE_DYN_FUNS \
    LBM_USE_DYN_ARRAYS \
    LBM_USE_DYN_DEFSTRUCT \
    LBM_USE_TIME_QUOTA \
    LBM_USE_ERROR_LINENO \
    LBM_USE_MACRO_REST_ARGS \
    LBM_USE_SHEBANG_COMMENTS

QT += core widgets

SOURCES += \
    $$PWD/src/platform_mutex.cpp \
    $$PWD/src/platform_thread.cpp \
    $$PWD/src/platform_timestamp.cpp

SOURCES += \
    $$LISPBM_ROOT/src/env.c \
    $$LISPBM_ROOT/src/fundamental.c \
    $$LISPBM_ROOT/src/heap.c \
    $$LISPBM_ROOT/src/lbm_memory.c \
    $$LISPBM_ROOT/src/print.c \
    $$LISPBM_ROOT/src/stack.c \
    $$LISPBM_ROOT/src/symrepr.c \
    $$LISPBM_ROOT/src/tokpar.c \
    $$LISPBM_ROOT/src/extensions.c \
    $$LISPBM_ROOT/src/lispbm.c \
    $$LISPBM_ROOT/src/eval_cps.c \
    $$LISPBM_ROOT/src/lbm_c_interop.c \
    $$LISPBM_ROOT/src/lbm_custom_type.c \
    $$LISPBM_ROOT/src/lbm_channel.c \
    $$LISPBM_ROOT/src/lbm_flat_value.c \
    $$LISPBM_ROOT/src/lbm_prof.c \
    $$LISPBM_ROOT/src/lbm_defrag_mem.c \
    $$LISPBM_ROOT/src/lbm_image.c \
    $$LISPBM_ROOT/utils/buffer.c \
    $$LISPBM_ROOT/utils/crypto.c \
    $$LISPBM_ROOT/utils/ecc.c

SOURCES += \
    $$LISPBM_ROOT/src/extensions/array_extensions.c \
    $$LISPBM_ROOT/src/extensions/string_extensions.c \
    $$LISPBM_ROOT/src/extensions/math_extensions.c \
    $$LISPBM_ROOT/src/extensions/runtime_extensions.c \
    $$LISPBM_ROOT/src/extensions/random_extensions.c \
    $$LISPBM_ROOT/src/extensions/set_extensions.c \
    $$LISPBM_ROOT/src/extensions/mutex_extensions.c \
    $$LISPBM_ROOT/src/extensions/lbm_dyn_lib.c \
    $$LISPBM_ROOT/src/extensions/crypto_extensions.c \
    $$LISPBM_ROOT/src/extensions/ecc_extensions.c \
    $$LISPBM_ROOT/src/extensions/display_extensions.c \
    $$LISPBM_ROOT/src/extensions/dsp_extensions.c

SOURCES += $$LISPBM_ROOT/src/extensions/tjpgd.c
SOURCES += $$LISPBM_ROOT/src/extensions/ttf_extensions.c
SOURCES += $$LISPBM_ROOT/src/extensions/ttf_backend.c

contains(DEFINES, LBM_TTF_USE_FREETYPE) {
  LIBS += -lfreetype
  SOURCES += $$LISPBM_ROOT/src/extensions/ttf_backend_freetype.c
} else {
  SOURCES += $$LISPBM_ROOT/src/extensions/schrift.c
}

LIBS += -lm

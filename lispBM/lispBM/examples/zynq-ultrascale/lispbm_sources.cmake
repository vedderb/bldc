# LispBM sources for AMD/Xilinx Vitis FreeRTOS projects
#
# Usage: Set LISPBM_DIR to the root of the lispbm repository, then
# include this file from your UserConfig.cmake:
#
#   set(LISPBM_DIR "/path/to/lispbm")
#   include("${LISPBM_DIR}/examples/zynq-ultrascale/lispbm_sources.cmake")
#
# Then append to the USER_ variables:
#
#   list(APPEND USER_COMPILE_SOURCES ${LISPBM_SOURCES})
#   list(APPEND USER_INCLUDE_DIRECTORIES ${LISPBM_INCLUDE_DIRS})

set(LISPBM_SOURCES
  "${LISPBM_DIR}/src/env.c"
  "${LISPBM_DIR}/src/eval_cps.c"
  "${LISPBM_DIR}/src/extensions.c"
  "${LISPBM_DIR}/src/fundamental.c"
  "${LISPBM_DIR}/src/heap.c"
  "${LISPBM_DIR}/src/lbm_memory.c"
  "${LISPBM_DIR}/src/lbm_channel.c"
  "${LISPBM_DIR}/src/lbm_c_interop.c"
  "${LISPBM_DIR}/src/lbm_custom_type.c"
  "${LISPBM_DIR}/src/lbm_defrag_mem.c"
  "${LISPBM_DIR}/src/lbm_flat_value.c"
  "${LISPBM_DIR}/src/lbm_image.c"
  "${LISPBM_DIR}/src/print.c"
  "${LISPBM_DIR}/src/stack.c"
  "${LISPBM_DIR}/src/symrepr.c"
  "${LISPBM_DIR}/src/tokpar.c"
  "${LISPBM_DIR}/src/lispbm.c"
  "${LISPBM_DIR}/platform/freertos/src/platform_mutex.c"
  "${LISPBM_DIR}/platform/freertos/src/platform_timestamp.c"
  "${LISPBM_DIR}/platform/freertos/src/platform_thread.c"
)

set(LISPBM_INCLUDE_DIRS
  "${LISPBM_DIR}/include"
  "${LISPBM_DIR}/platform/freertos/include"
)

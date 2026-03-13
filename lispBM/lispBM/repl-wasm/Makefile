LISPBM = ..

EMCC = emcc

CFLAGS = -O2 \
         -I$(LISPBM)/include \
         -I$(LISPBM)/include/extensions \
         -I$(LISPBM)/src \
         -I$(LISPBM)/platform/wasm/include

LBMFLAGS = -DLBM_SINGLE_THREADED \
           -DLBM_USE_DYN_MACROS \
	   -DLBM_USE_DYN_FUNS \
           -DLBM_USE_DYN_ARRAYS \
           -DLBM_USE_DYN_LOOPS \
           -DLBM_USE_DYN_DEFSTRUCT

EMFLAGS = \
  -s SUPPORT_LONGJMP=emscripten \
  -s ASSERTIONS=1 \
  -s MODULARIZE=1 \
  -s EXPORT_NAME=LispBM \
  -s EXPORTED_FUNCTIONS='["_lbm_wasm_init","_lbm_wasm_eval","_lbm_wasm_eval_program","_lbm_wasm_step","_lbm_wasm_run","_lbm_wasm_get_output","_lbm_wasm_clear_output","_lbm_wasm_is_running"]' \
  -s EXPORTED_RUNTIME_METHODS='["ccall","cwrap"]' \
  -s INITIAL_MEMORY=16777216 \
  -s ALLOW_MEMORY_GROWTH=1

LISPBM_SRC = \
  $(LISPBM)/src/env.c \
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
  $(LISPBM)/src/lbm_flat_value.c \
  $(LISPBM)/src/lbm_prof.c \
  $(LISPBM)/src/lbm_defrag_mem.c \
  $(LISPBM)/src/lbm_image.c \
  $(LISPBM)/src/buffer.c \
  $(LISPBM)/src/extensions/array_extensions.c \
  $(LISPBM)/src/extensions/string_extensions.c \
  $(LISPBM)/src/extensions/math_extensions.c \
  $(LISPBM)/src/extensions/runtime_extensions.c \
  $(LISPBM)/src/extensions/random_extensions.c \
  $(LISPBM)/src/extensions/set_extensions.c \
  $(LISPBM)/src/extensions/lbm_dyn_lib.c \
  $(LISPBM)/src/extensions/crypto_extensions.c \
  $(LISPBM)/platform/wasm/src/platform_timestamp.c

SRC = repl.c

all: lispbm.js

lispbm.js: $(LISPBM_SRC) $(SRC)
	$(EMCC) $(CFLAGS) $(LBMFLAGS) $(EMFLAGS) $(LISPBM_SRC) $(SRC) -lm -o lispbm.js

clean:
	rm -f lispbm.js lispbm.wasm

# WASM requires HTTP - cannot be opened as file://
serve:
	python3 -m http.server 8080

publish:
	cp index.html ../../lispbm.com/repl-wasm
	cp lispbm.js  ../../lispbm.com/repl-wasm
	cp lispbm.wasm ../../lispbm.com/repl-wasm

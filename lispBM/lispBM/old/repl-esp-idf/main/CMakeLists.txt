idf_component_register(
  SRCS "hello_world_main.c"
       "../../src/heap.c"
       "../../src/env.c"
       "../../src/extensions.c"
       "../../src/fundamental.c"
       "../../src/eval_cps.c"
       "../../src/memory.c"
       "../../src/prelude.c"
       "../../src/print.c"
       "../../src/qq_expand.c"
       "../../src/stack.c"
       "../../src/symrepr.c"
       "../../src/tokpar.c"
  
  INCLUDE_DIRS ""
               "../../include"
               "../../src"
       )

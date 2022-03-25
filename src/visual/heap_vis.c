
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "heap.h"


static uint32_t img_cnt=0;

typedef struct {
  uint8_t r;
  uint8_t g;
  uint8_t b;
} rgb_t;

rgb_t rgb(uint8_t r, uint8_t g, uint8_t b) {
  rgb_t res;
  res.r = r;
  res.g = g;
  res.b = b;
  return res;
}

int heap_vis_init(void) {

  
  return 1; 
}



void heap_vis_gen_image(void) {

  lbm_heap_state_t hs;
  lbm_get_heap_state(&hs);
  
  uint32_t num_pix = hs.heap_size;
  uint32_t i;

  rgb_t free_color   = rgb(0,255,0);
  rgb_t marked_color = rgb(255,0,0);
  rgb_t used_color   = rgb(0,0,255);
  
  rgb_t *pix_data = malloc(num_pix * sizeof(rgb_t));

  if (pix_data == NULL) return; 
  
  uint32_t *heap = (uint32_t*)hs.heap;

  for (i = 0; i < num_pix*2; i +=2 ) {

    //uint32_t car = heap[i];
    uint32_t cdr = heap[i+1];
    rgb_t col = used_color; 

    
    
    if ((cdr & LBM_GC_MASK) == LBM_GC_MARKED) {
      col = marked_color; 
    }

    pix_data[i/2] = col; 
  }

  uint32_t fl = hs.freelist; 

  while (lbm_type_of(fl) == LBM_TYPE_CONS) {
    uint32_t index = lbm_dec_ptr(fl);
    pix_data[index] = free_color; 
    fl = lbm_cdr(fl);
  }
  
  char fn[256];
  memset(fn,0,256);
  snprintf(fn, 256, "img_%07d.raw",img_cnt++);

  FILE *fp = fopen(fn, "w");

  fwrite(pix_data,3,num_pix,fp);

  fclose(fp);
  free(pix_data); 
  
}

#ifndef HEAP_VIS_H_
#define HEAP_VIS_H_

#ifdef __cplusplus
extern "C" {
#endif

/** \file heap_vis.h */

 /** Create an image representing the current state of the heap.
  *   This function is not meant for execution on an microcontroller.
  */
void heap_vis_gen_image(void);

#ifdef __cplusplus
}
#endif
#endif

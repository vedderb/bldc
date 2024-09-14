/*
    Copyright 2022 Joel Svensson  svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>

#include <stdio.h>
#include <unistd.h>
#include <stdbool.h>

#include <lispbm.h>
#include <lbm_custom_type.h>
#include <extensions/display_extensions.h>


typedef struct {
  uint32_t sdl_id;
  char *name;
  lbm_uint sym_id;
} sdl_symbol_t;

sdl_symbol_t lbm_sdl_events[] = {
  {0       , "sdl-no-event", 0},
  {SDL_QUIT, "sdl-quit-event", 0},
  {SDL_KEYDOWN, "sdl-key-down-event", 0},
  {SDL_KEYUP, "sdl-key-up-event", 0}
};


static int register_sdl_event_symbols(void) {
  for (size_t i = 0; i < (sizeof(lbm_sdl_events) / sizeof(sdl_symbol_t)); i ++) {
    if (!lbm_add_symbol(lbm_sdl_events[i].name, &lbm_sdl_events[i].sym_id))
      return 0;
  }
  return 1;
}

static lbm_uint lookup_sdl_event_symbol(uint32_t sdl_event) {
  for (size_t i = 0; i < (sizeof(lbm_sdl_events) / sizeof(sdl_symbol_t)); i ++) {
    if (sdl_event == lbm_sdl_events[i].sdl_id) {
      return lbm_sdl_events[i].sym_id;
    }
  }
  return lbm_sdl_events[0].sym_id;
}

static lbm_value ext_sdl_init(lbm_value *args, lbm_uint argn) {
  // TODO init differently depending on args
  lbm_value res = lbm_enc_sym(SYM_NIL);

  if ((SDL_Init(SDL_INIT_EVERYTHING) == 0) &&
      (IMG_Init(IMG_INIT_PNG | IMG_INIT_JPG))) {
    res = lbm_enc_sym(SYM_TRUE);
  }
  return res;
}

static bool sdl_window_destructor(lbm_uint value) {
  SDL_DestroyWindow((SDL_Window*)value);
  return true;
}

static lbm_value ext_sdl_create_window(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_NIL);

  if (argn == 3) {

    char *title = lbm_dec_str(args[0]);

    if (title) {

      int32_t w = lbm_dec_as_i32(args[1]);
      int32_t h = lbm_dec_as_i32(args[2]);

      SDL_Window* win = SDL_CreateWindow(title,
					 SDL_WINDOWPOS_CENTERED,
					 SDL_WINDOWPOS_CENTERED,
					 w, h, 0);

      if (win && !lbm_custom_type_create((lbm_uint)win, sdl_window_destructor, "SDLWindow", &res)) {
	SDL_DestroyWindow(win);
      }
    }
  }
  return res;
}

static bool sdl_renderer_destructor(lbm_uint value) {
  SDL_DestroyRenderer((SDL_Renderer*)value);
  return true;
}

static lbm_value ext_sdl_create_soft_renderer(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_NIL);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {

    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);

    SDL_Renderer* rend = SDL_CreateRenderer((SDL_Window *)m[CUSTOM_TYPE_VALUE],
                                            -1,SDL_RENDERER_SOFTWARE);
    if (rend && !lbm_custom_type_create((lbm_uint)rend, sdl_renderer_destructor, "SDLRenderer", &res)) {
      SDL_DestroyRenderer(rend);
    }
  }
  return res;
}

static lbm_value ext_sdl_renderer_set_color(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 4 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer *)m[CUSTOM_TYPE_VALUE];

    uint8_t r = lbm_dec_as_char(args[1]);
    uint8_t g = lbm_dec_as_char(args[2]);
    uint8_t b = lbm_dec_as_char(args[3]);

    /* SDL functions are 0 on success */
    if (SDL_SetRenderDrawColor(rend, r, g, b, SDL_ALPHA_OPAQUE)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}


static lbm_value ext_sdl_draw_line(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 5 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    int32_t x1 = lbm_dec_as_i32(args[1]);
    int32_t y1 = lbm_dec_as_i32(args[2]);
    int32_t x2 = lbm_dec_as_i32(args[3]);
    int32_t y2 = lbm_dec_as_i32(args[4]);

    if (SDL_RenderDrawLine(rend, x1, y1, x2, y2)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}

static lbm_value ext_sdl_draw_point(lbm_value *args, lbm_uint argn) {

  lbm_value res = ENC_SYM_EERROR;

  if (argn == 3 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    int32_t x1 = lbm_dec_as_i32(args[1]);
    int32_t y1 = lbm_dec_as_i32(args[2]);
    res = ENC_SYM_TRUE;
    if (SDL_RenderDrawPoint(rend, x1, y1)) {
      res = ENC_SYM_NIL;
    }
  }
  return res;
}


static lbm_value ext_sdl_clear(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    if (SDL_RenderClear(rend)) {
      res = lbm_enc_sym(SYM_NIL);
    }
  }
  return res;
}

static lbm_value ext_sdl_present(lbm_value *args, lbm_uint argn) {

  lbm_value res = lbm_enc_sym(SYM_TRUE);

  if (argn == 1 && lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    SDL_RenderPresent(rend);
  }
  return res;
}


static lbm_value ext_sdl_poll_event(lbm_value *args, lbm_uint argn) {

  SDL_Event event;

  if (SDL_PollEvent(&event) == 0)
    return lbm_enc_sym(lookup_sdl_event_symbol(0));
  return lbm_enc_sym(lookup_sdl_event_symbol(event.type));
}


////////////////////////////////////////////////////////////
// Images and Textures

static bool sdl_texture_destructor(lbm_uint value) {
  SDL_DestroyTexture((SDL_Texture*)value);
  return true;
}

static lbm_value ext_sdl_load_texture(lbm_value *args, lbm_uint argn) {
  lbm_value res = lbm_enc_sym(SYM_NIL);
  if (argn == 2 &&
      (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) &&
      (lbm_type_of(args[1]) == LBM_TYPE_ARRAY)) {
    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];
    char *filename = lbm_dec_str(args[1]);
    if (rend &&
	filename) {

      SDL_Texture *texture = IMG_LoadTexture(rend, filename);
      if (texture &&
	  !lbm_custom_type_create((lbm_uint)texture, sdl_texture_destructor, "SDLTexture", &res))
	SDL_DestroyRenderer(rend);
    }
  }
  return res;
}

static lbm_value ext_sdl_blit(lbm_value *args, lbm_uint argn) {
  lbm_value res = lbm_enc_sym(SYM_NIL);
  if (argn == 6 &&
      (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM) &&
      (lbm_type_of(args[1]) == LBM_TYPE_CUSTOM) &&
      lbm_is_number(args[2]) && lbm_is_number(args[3]) &&
      lbm_is_number(args[4]) && lbm_is_number(args[5])) {

    int32_t x = lbm_dec_as_i32(args[2]);
    int32_t y = lbm_dec_as_i32(args[3]);
    int32_t w = lbm_dec_as_i32(args[4]);
    int32_t h = lbm_dec_as_i32(args[5]);

    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];
    lbm_uint *t = (lbm_uint *)lbm_dec_custom(args[1]);
    SDL_Texture *texture = (SDL_Texture*)t[CUSTOM_TYPE_VALUE];

    if (rend && texture) {
      SDL_Rect dest = { x, y, w, h };

      SDL_RenderCopy(rend, texture, NULL, &dest);
      res = lbm_enc_sym(SYM_TRUE);
    }
  }
  return res;
}

// Display interface


SDL_Renderer *active_rend = NULL;

static lbm_value ext_sdl_set_active_renderer(lbm_value *args, lbm_uint argn) {
  lbm_value res = lbm_enc_sym(SYM_NIL);
  if (argn == 1 &&
      (lbm_type_of(args[0]) == LBM_TYPE_CUSTOM)) {

    lbm_uint *m = (lbm_uint *)lbm_dec_custom(args[0]);
    SDL_Renderer *rend = (SDL_Renderer*)m[CUSTOM_TYPE_VALUE];

    active_rend = rend;
    res = ENC_SYM_TRUE;
  }
  return res;
}

// hacky
static void blast_indexed2(uint8_t *dest, int dest_pitch, image_buffer_t *img, color_t *colors) {

  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 3;
    int bit  = 7 - (i & 0x7);
    int color_ind = (data[byte] & (1 << bit)) >> bit;

    uint32_t color = COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    w_dest[i] = color;
  }
}

static void blast_indexed4(uint8_t *dest, int dest_pitch, image_buffer_t *img, color_t *colors) {
  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 2;
    int bit = (3 - (i & 0x03)) * 2;
    int color_ind = (data[byte] & (0x03 << bit)) >> bit;

    uint32_t color = COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    w_dest[i] = color;
  }
}

static void blast_indexed16(uint8_t *dest, int dest_pitch,image_buffer_t *img, color_t *colors) {
  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    int byte = i >> 1;    // byte to access is pix / 2
    int bit = (1 - (i & 0x01)) * 4; // bit position to access within byte
    int color_ind = (data[byte] & (0x0F << bit)) >> bit; // extract 4 bit value.

    uint32_t color = COLOR_TO_RGB888(colors[color_ind],
                                     i % w, i / w);
    w_dest[i] = color;
  }
}

static void blast_rgb332(uint8_t *dest, int dest_pitch,image_buffer_t *img) {
  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    uint8_t pix = data[i];
    uint32_t r = (uint32_t)((pix >> 5) & 0x7);
    uint32_t g = (uint32_t)((pix >> 2) & 0x7);
    uint32_t b = (uint32_t)(pix & 0x3);
    uint32_t rgb888 = r << (16 + 5) | g << (8 + 5) | b << 6;
    w_dest[i] = rgb888;
  }
}

static void blast_rgb565(uint8_t *dest, int dest_pitch, image_buffer_t *img) {
  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    uint16_t pix = (((uint16_t)data[2 * i]) << 8) | ((uint16_t)data[2 * i + 1]);

    uint32_t r = (uint32_t)(pix >> 11);
    uint32_t g = (uint32_t)((pix >> 5) & 0x3F);
    uint32_t b = (uint32_t)(pix & 0x1F);
    uint32_t rgb888 = r << (16 + 3) | g << (8 + 2) | b << 3;
    w_dest[i] = rgb888;
  }
}

static void blast_rgb888(uint8_t *dest, int dest_pitch, image_buffer_t *img) {
  uint8_t *data = img->data;
  uint16_t w    = img->width;
  uint16_t h    = img->height;
  int num_pix = w * h;

  uint32_t *w_dest = (uint32_t *)dest;
  for (int i = 0; i < num_pix; i ++) {
    uint32_t r = data[3 * i];
    uint32_t g = data[3 * i + 1];
    uint32_t b = data[3 * i + 2];

    uint32_t rgb888 = r << 16 | g << 8 | b;
    w_dest[i] = rgb888;
  }
}


bool sdl_render_image(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {

  if (active_rend) {

    uint16_t w = img->width;
    uint16_t h = img->height;
    uint8_t  bpp = img->fmt;

    SDL_Texture* tex = SDL_CreateTexture(active_rend, SDL_PIXELFORMAT_RGB888,SDL_TEXTUREACCESS_STREAMING, w, h);
    int pitch = 0;

    uint8_t* p = NULL;
    SDL_LockTexture(tex, NULL, (void**)&p, &pitch);
    switch(bpp) {
    case indexed2:
      blast_indexed2(p, pitch, img, colors);
      break;
    case indexed4:
      blast_indexed4(p, pitch, img, colors);
      break;
    case indexed16:
      blast_indexed16(p, pitch, img, colors);
      break;
    case rgb332:
      blast_rgb332(p, pitch, img);
      break;
    case rgb565:
      blast_rgb565(p, pitch, img);
      break;
    case rgb888:
      blast_rgb888(p, pitch, img);
      break;
    default:
      break;
    }

    SDL_UnlockTexture(tex);

    SDL_Rect dest = { x, y, w, h };
    SDL_RenderCopy(active_rend, tex, NULL, &dest);
    SDL_RenderPresent(active_rend);
  }
  return true;
}


void sdl_clear(uint32_t color) {
  if (active_rend) {
    SDL_RenderClear(active_rend);
    SDL_RenderPresent(active_rend);
  }
}

void sdl_reset(void) {
  return;
}


////////////////////////////////////////////////////////////
// Active LBM SDL extensions
bool lbm_sdl_init(void) {

  register_sdl_event_symbols();

  lbm_add_extension("sdl-init", ext_sdl_init);
  lbm_add_extension("sdl-create-window",ext_sdl_create_window);
  lbm_add_extension("sdl-create-soft-renderer", ext_sdl_create_soft_renderer);
  lbm_add_extension("sdl-renderer-set-color", ext_sdl_renderer_set_color);
  lbm_add_extension("sdl-draw-point", ext_sdl_draw_point);
  lbm_add_extension("sdl-draw-line", ext_sdl_draw_line);
  lbm_add_extension("sdl-clear", ext_sdl_clear);
  lbm_add_extension("sdl-present", ext_sdl_present);
  lbm_add_extension("sdl-poll-event", ext_sdl_poll_event);

  lbm_add_extension("sdl-load-texture", ext_sdl_load_texture);
  lbm_add_extension("sdl-blit", ext_sdl_blit);

  lbm_add_extension("sdl-set-active-renderer", ext_sdl_set_active_renderer);
  lbm_display_extensions_init();
  lbm_display_extensions_set_callbacks(sdl_render_image,
                                       sdl_clear,
                                       sdl_reset);

  return lbm_get_num_extensions() < lbm_get_max_extensions();
}

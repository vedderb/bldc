
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "ch.h"
#include "hal.h"

#include "chprintf.h"

#include "usbcfg.h"

#include <lispbm.h>
#include <lbm_image.h>

// This "chp" stream is a kind of handle for
// reading and writing via the USB
BaseSequentialStream *chp = NULL;

// LispBM setting and state
#define WAIT_TIMEOUT 2500
#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(1024)
#define EVAL_CPS_STACK_SIZE 256
#define GC_STACK_SIZE 256
#define PRINT_STACK_SIZE 256
#define HEAP_SIZE 4096
#define EXTENSION_STORAGE_SIZE 256

static lbm_extension_t extensions[EXTENSION_STORAGE_SIZE];

static lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

static uint32_t memory_array[LBM_MEMORY_SIZE_8K];
static uint32_t bitmap_array[LBM_MEMORY_BITMAP_SIZE_8K];

static lbm_string_channel_state_t string_tok_state;
static lbm_char_channel_t string_tok;


// Flash sector 11 is 128KB (has to be cleared all in one go!)
#define FLASH_SECTOR_11_ADDR    0x080E0000
#define FLASH_SECTOR_11_SIZE    (128 * 1024)
#define IMAGE_SIZE_WORDS        (FLASH_SECTOR_11_SIZE / 4)  // 32K words

// STM32F4 Flash Control Register addresses
// Note: FLASH_R_BASE is the flash peripheral registers (0x40023C00)
//       FLASH_BASE is the flash memory area (0x08000000)
#define MY_FLASH_ACR            (*(volatile uint32_t *)(FLASH_R_BASE + 0x00))
#define MY_FLASH_KEYR           (*(volatile uint32_t *)(FLASH_R_BASE + 0x04))
#define MY_FLASH_OPTKEYR        (*(volatile uint32_t *)(FLASH_R_BASE + 0x08))
#define MY_FLASH_SR             (*(volatile uint32_t *)(FLASH_R_BASE + 0x0C))
#define MY_FLASH_CR             (*(volatile uint32_t *)(FLASH_R_BASE + 0x10))
#define MY_FLASH_OPTCR          (*(volatile uint32_t *)(FLASH_R_BASE + 0x14))

// Flash Control Register bits
#define FLASH_CR_SNB_POS        3          // Sector Number position
#define FLASH_CR_PSIZE_32       (2 << 8)   // Program size: 32-bit

#define FLASH_KEY1              0x45670123
#define FLASH_KEY2              0xCDEF89AB

static void flash_unlock(void) {
  MY_FLASH_KEYR = FLASH_KEY1;
  MY_FLASH_KEYR = FLASH_KEY2;
}

static void flash_lock(void) {
  MY_FLASH_CR |= FLASH_CR_LOCK;
}

static void flash_wait_busy(void) {
  while (MY_FLASH_SR & FLASH_SR_BSY);
}

static bool flash_erase_sector_11(void) {
  flash_unlock();
  flash_wait_busy();

  MY_FLASH_CR = FLASH_CR_SER | (11 << FLASH_CR_SNB_POS) | FLASH_CR_PSIZE_32;
  MY_FLASH_CR |= FLASH_CR_STRT;

  flash_wait_busy();

  MY_FLASH_SR = FLASH_SR_EOP;

  flash_lock();
  return true;
}

static bool flash_write_word(uint32_t addr, uint32_t data) {
  flash_unlock();
  flash_wait_busy();

  MY_FLASH_CR = FLASH_CR_PG | FLASH_CR_PSIZE_32;

  *(volatile uint32_t *)addr = data;

  flash_wait_busy();

  MY_FLASH_SR = FLASH_SR_EOP;

  flash_lock();
  return true;
}

// LispBM image callbacks
static bool image_write(uint32_t w, int32_t ix, bool const_heap) {
  (void)const_heap;
  if (ix < 0 || ix >= IMAGE_SIZE_WORDS) {
    return false;
  }

  uint32_t current = *((uint32_t*)(FLASH_SECTOR_11_ADDR + (ix * 4)));
  if (current == w) {
    return true;
  }
  if (current != 0xFFFFFFFF) {
    return false;
  }
  return flash_write_word(FLASH_SECTOR_11_ADDR + (ix * 4), w);
}


////////////////////////////////////////////////////////////
// Lispbm readline
int inputline(BaseSequentialStream *chp, char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = streamGet(chp);
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      streamPut(chp,0x8); /* output backspace character */
      streamPut(chp,' ');
      streamPut(chp,0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        streamPut(chp,c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

#define BUF_SIZE 1024
static char inbuf[BUF_SIZE];
static char outbuf[BUF_SIZE];


void sleep_callback(uint32_t us) {
  chThdSleepMicroseconds(us);
}

void done_callback(eval_context_t *ctx) {
  char *output = outbuf;
  lbm_value t = ctx->r;

  lbm_print_value(output, BUF_SIZE, t);
  chprintf(chp,"%s\r\n", output);
}

static THD_FUNCTION(eval, arg) {
  (void) arg;
  lbm_run_eval();
}

lbm_value ext_print(lbm_value *args, lbm_uint argn) {

  char *output = outbuf;

  for (lbm_uint i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && lbm_type_of(t) == LBM_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      chprintf(chp,"%s", (char*)array + 8);
    } else if (lbm_type_of(t) == LBM_TYPE_CHAR) {
      if (lbm_dec_char(t) =='\n') {
        chprintf(chp, "\r\n");
      } else {
        chprintf(chp,"%c", lbm_dec_char(t));
      }
    }  else {
      lbm_print_value(output, BUF_SIZE, t);
      chprintf(chp,"%s", output);
    }
  }
  chprintf(chp,"\r\n");
  return lbm_enc_sym(SYM_TRUE);
}

lbm_value ext_image_save(lbm_value *args, lbm_uint argn) {
  (void) args;
  (void) argn;
  bool r = true;

  lbm_uint main_sym = ENC_SYM_NIL;
  if (lbm_get_symbol_by_name("main", &main_sym)) {
    lbm_value binding;
    if (lbm_global_env_lookup(&binding, lbm_enc_sym(main_sym))) {
      if (lbm_is_cons(binding) && lbm_car(binding) == ENC_SYM_CLOSURE) {
        goto image_has_main;
      }
    }
  }
  lbm_set_error_reason("No main function in image\n");
  return ENC_SYM_EERROR;


 image_has_main:
  r = r && lbm_image_save_global_env();
  r = r && lbm_image_save_extensions();
  r = r && lbm_image_save_constant_heap_ix();

  return r ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

lbm_value ext_image_clear(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;

  chprintf(chp, "Clearing LispBM image and restarting...\r\n");
  chThdSleepMilliseconds(100);

  flash_erase_sector_11();
  NVIC_SystemReset();

  return ENC_SYM_TRUE; // not reached
}

lbm_value ext_reset(lbm_value *args, lbm_uint argn) {
  (void)args;
  (void)argn;

  chprintf(chp, "Saving const heap index and resetting...\r\n");
  chThdSleepMilliseconds(100);

  // Save const heap state before reset
  lbm_image_save_constant_heap_ix();
  NVIC_SystemReset();

  return ENC_SYM_TRUE; // not reached
}


static void context_exists(eval_context_t *ctx, void *cid, void *b) {
  if (ctx->id == *(lbm_cid*)cid) {
    *(bool*)b = true;
  }
}

static bool lbm_wait_ctx(lbm_cid cid, lbm_uint timeout_ms) {

  bool exists;
  uint32_t i = 0;

  do {
    exists = false;
    lbm_blocked_iterator(context_exists, &cid, &exists);
    lbm_running_iterator(context_exists, &cid, &exists);

    eval_context_t *ctx_running = lbm_get_current_context();

    if (ctx_running &&
        ctx_running->id == cid) {
      exists = true;
    }

    if (exists) {
      sleep_callback(10);
      if (timeout_ms > 0) i ++;
    }
  } while (exists && i < timeout_ms);

  if (exists) return false;
  return true;
}

int main(void) {
  halInit();
  chSysInit();

  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);

  // Disable VBUS sensing for STM32F4-Discovery
  serusbcfg.usbp->otg->GCCFG |= USB_OTG_GCCFG_NOVBUSSENS;
  serusbcfg.usbp->otg->GCCFG &= ~USB_OTG_GCCFG_VBUSBSEN;
  serusbcfg.usbp->otg->GCCFG &= ~USB_OTG_GCCFG_VBUSASEN;

  usbConnectBus(serusbcfg.usbp);

  chp = (BaseSequentialStream*)&SDU1;

  chThdSleepMilliseconds(2000);

  if (!lbm_init(heap, HEAP_SIZE,
                memory_array, LBM_MEMORY_SIZE_8K,
                bitmap_array, LBM_MEMORY_BITMAP_SIZE_8K,
                GC_STACK_SIZE,
                PRINT_STACK_SIZE,
                extensions,
                EXTENSION_STORAGE_SIZE)) {
    chprintf(chp,"LispBM Init failed.\r\n");
    return 0;
  }

  lbm_set_ctx_done_callback(done_callback);
  lbm_set_usleep_callback(sleep_callback);

  lbm_image_init((uint32_t*)FLASH_SECTOR_11_ADDR,
                 IMAGE_SIZE_WORDS,
                 image_write);

  if (!lbm_image_exists()) {
    chprintf(chp,"Image does not exist - creating!\r\n");
    chprintf(chp,"Erasing flash\r\n");
    flash_erase_sector_11();
    chprintf(chp,"Creating image\r\n");
    lbm_image_create("stm32");
  }

  if (!lbm_image_boot()) {
    chprintf(chp,"Unable to boot image.\r\n");
    while (1);
  }
  chprintf(chp,"Image booted\r\n");

  uint32_t *flash_ptr = (uint32_t *)FLASH_SECTOR_11_ADDR;
  lbm_uint const_heap_ix = lbm_image_const_heap_index();

  if (lbm_image_exists() && flash_ptr[const_heap_ix] != 0xFFFFFFFF) {
    chprintf(chp,"WARNING: Image corrupted! const_heap_ix=%d points to 0x%08X (expected 0xFFFFFFFF)\r\n",
             (unsigned int)const_heap_ix, (unsigned int)flash_ptr[const_heap_ix]);
    chprintf(chp,"Clearing and resetting!\r\n");
    flash_erase_sector_11();
    NVIC_SystemReset();
  }

  if (!lbm_image_has_extensions()) {
    lbm_add_extension("print", ext_print);
    lbm_add_extension("image-save", ext_image_save);
    lbm_add_extension("image-clear", ext_image_clear);
    lbm_add_extension("reset", ext_reset);
  }

  thread_t *t = chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
                                    "eval", NORMALPRIO+1,
                                    eval, (void *)NULL);

  if (!t) {
    chprintf(chp,"Error starting evaluator thread.\r\n");
    return 0;
  }
  chprintf(chp,"Lisp REPL started (ChibiOS)!\r\n");

  // Check if there is a main function in the env
  // and launch it!
  lbm_uint main_sym = ENC_SYM_NIL;
  if (lbm_get_symbol_by_name("main", &main_sym)) {
    lbm_value binding;
    if (lbm_global_env_lookup(&binding, lbm_enc_sym(main_sym))) {
      if (lbm_is_cons(binding) && lbm_car(binding) == ENC_SYM_CLOSURE) {
        lbm_pause_eval();
        while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }
        lbm_create_string_char_channel(&string_tok_state, &string_tok, "(main)");
        lbm_cid cid = lbm_load_and_eval_expression(&string_tok);
        lbm_continue_eval();
        lbm_wait_ctx((lbm_cid)cid, WAIT_TIMEOUT);
      }
    }
  }

  while (true) {
    chprintf(chp, "# ");
    memset(inbuf,0, BUF_SIZE);
    memset(outbuf, 0, BUF_SIZE);
    inputline(chp,inbuf, BUF_SIZE);
    chprintf(chp,"\r\n");

    if (strlen(inbuf) == 0) {
      continue;
    }

    /* Get exclusive access to the heap */
    lbm_pause_eval();
    while(lbm_get_eval_state() != EVAL_CPS_STATE_PAUSED) {
      sleep_callback(10);
    }

    lbm_create_string_char_channel(&string_tok_state,
                                   &string_tok,
                                   inbuf);

    lbm_cid cid = lbm_load_and_eval_expression(&string_tok);

    lbm_continue_eval();

    lbm_wait_ctx((lbm_cid)cid, WAIT_TIMEOUT);
  }
}

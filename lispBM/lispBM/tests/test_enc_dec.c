
#include <stdlib.h>
#include <stdio.h>

#include "heap.h"

int main(int argc, char **argv) {
  (void) argc;
  (void) argv;

  int res = 1;

  int n = 0;
  res &= (lbm_dec_i(lbm_enc_i(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_i(lbm_enc_i(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_i(lbm_enc_i(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_i(lbm_enc_i(134217727)) == 134217727);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_i(lbm_enc_i(-134217728)) == -134217728);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (lbm_dec_u(lbm_enc_u(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_u(lbm_enc_u(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_u(lbm_enc_u(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (lbm_dec_char(lbm_enc_char(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_char(lbm_enc_char(-1)) == -1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_char(lbm_enc_char(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_char(lbm_enc_char(127)) == 127);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_char(lbm_enc_char(-128)) == -128);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");

  res &= (lbm_dec_sym(lbm_enc_sym(0)) == 0);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_sym(lbm_enc_sym(1)) == 1);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  res &= (lbm_dec_sym(lbm_enc_sym(268435455)) == 268435455);
  printf("DEC/ENC %d: %s \n", n++, res ? "ok" : "NOK!");
  
  return res;
}

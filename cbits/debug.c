#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <immintrin.h>
#include <mmintrin.h>

#include "json.h"

void print256_num(__m256i var) {
  uint8_t *val = (uint8_t*)&var;
  printf("Numerical: "
    "%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x "
    "%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x \n"
    , val[ 0], val[ 1], val[ 2], val[ 3], val[ 4], val[ 5], val[ 6], val[ 7]
    , val[ 8], val[ 9], val[10], val[11], val[12], val[13], val[14], val[15]
    , val[16], val[17], val[18], val[19], val[20], val[21], val[22], val[23]
    , val[24], val[25], val[26], val[27], val[28], val[29], val[30], val[31]);
}

void print_bits_64(uint64_t v) {
  char *digits = "01";

  for (int i = 0; i < 64; ++i) {
    printf("%c", digits[(v >> i) & 1]);
  }
}
#include "io.h"

#include <stdlib.h>
#include <stdio.h>

void io_integer(int value) {
  printf("%d", value);
}

void io_real(float value) {
  printf("%f", value);
}

void io_string(const char *value) {
  printf("%s", value);
}

void io_panic(const char *message) {
  printf("panic: %s", message);
  exit(1);
}

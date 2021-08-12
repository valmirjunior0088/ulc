#include "io.h"

#include <stdlib.h>
#include <stdio.h>

void panic(const char *message) {
  printf("panic: %s", message);
  exit(1);
}

void print_integer(int value) {
  printf("%d", value);
}

void print_real(float value) {
  printf("%f", value);
}

void print_string(const char *value) {
  printf("%s", value);
}

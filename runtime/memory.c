#include "memory.h"

#include <stdlib.h>

void *alloc(unsigned size) {
  return malloc(size);
}

void dealloc(void *pointer) {
  return free(pointer);
}


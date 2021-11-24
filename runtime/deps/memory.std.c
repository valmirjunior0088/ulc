#include "memory.h"

#include <stdlib.h>

void *alloc(size_t size) {
  return malloc(size);
}

void dealloc(void *pointer) {
  return free(pointer);
}


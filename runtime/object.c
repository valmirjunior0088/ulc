#include "object.h"

#include "io.h"
#include "memory.h"

enum type {
  INTEGER,
  REAL,
  CLOSURE,
};

struct integer {
  int value;
};

struct real {
  float value;
};

struct closure {
  abstraction abstraction;
  unsigned capacity;
  struct object *values[];
};

union payload {
  struct integer integer;
  struct real real;
  struct closure closure;
};

struct object {
  unsigned reference_count;
  enum type type;
  union payload payload;
};

void assert_reference_count(struct object *object) {
  if (object->reference_count <= 0) {
    panic("object had invalid reference count");
  }
}

void assert_type(struct object *object, enum type type) {
  if (object->type != type) {
    panic("object had wrong type");
  }
}

void object_enter(struct object *object) {
  assert_reference_count(object);
  object->reference_count++;
}

void object_leave(struct object *object) {
  assert_reference_count(object);
  object->reference_count--;

  if (object->reference_count > 0) {
    return;
  }

  if (object->type == CLOSURE) {
    for (int index = 0; index < object->payload.closure.capacity; index++) {
      object_leave(object->payload.closure.values[index]);
    }
  }

  dealloc(object);
}

struct object *object_integer(int value) {
  struct object *object = alloc(sizeof(struct object));
  object->reference_count = 1;
  object->type = INTEGER;
  object->payload.integer.value = value;

  return object;
}

struct object *object_real(float value) {
  struct object *object = alloc(sizeof(struct object));
  object->reference_count = 1;
  object->type = REAL;
  object->payload.real.value = value;

  return object;
}

struct object *object_closure(
  abstraction abstraction,
  unsigned capacity,
  struct object *values[]
) {
  struct object *object = alloc(
    sizeof(struct object) + sizeof(struct object *) * capacity
  );

  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = capacity;

  for (int index = 0; index < capacity; index++) {
    object->payload.closure.values[index] = values[index];
  }

  return object;
}

struct object *object_apply(struct object *function, struct object *argument) {
  assert_type(function, CLOSURE);

  return function->payload.closure.abstraction(
    function->payload.closure.values,
    argument
  );
}

struct object *object_integer_sum(struct object *left, struct object *right) {
  assert_type(left, INTEGER);
  assert_type(right, INTEGER);

  return object_integer(
    left->payload.integer.value + right->payload.integer.value
  );
}

struct object *object_real_sum(struct object *left, struct object *right) {
  assert_type(left, REAL);
  assert_type(right, REAL);

  return object_real(
    left->payload.real.value + right->payload.real.value
  );
}

void object_debug(struct object *object) {
  print_string("{ ");

  switch (object->type) {
    case INTEGER:
      print_string("INTEGER ");
      print_integer(object->payload.integer.value);
      break;

    case REAL:
      print_string("REAL ");
      print_real(object->payload.real.value);
      break;
    
    case CLOSURE:
      print_string("CLOSURE [ ");

      for (int index = 0; index < object->payload.closure.capacity; index++) {
        object_debug(object->payload.closure.values[index]);
      }

      print_string(" ]");
      break;
  }

  print_string(" }");
}

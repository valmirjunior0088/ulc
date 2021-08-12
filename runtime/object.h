#ifndef OBJECT_H
#define OBJECT_H

struct object;

typedef struct object *(*abstraction)(struct object *[], struct object *);

void object_enter(struct object *);
void object_leave(struct object *);

struct object *object_integer(int);
struct object *object_real(float);
struct object *object_closure(abstraction, unsigned, struct object *[]);
struct object *object_apply(struct object *, struct object *);

struct object *object_integer_sum(struct object *, struct object *);
struct object *object_real_sum(struct object *, struct object *);

void object_debug(struct object *);

#endif

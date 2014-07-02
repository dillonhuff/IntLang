#ifndef BASIC_RUNTIME_H
#define BASIC_RUNTIME_H

struct Comp {
  int eval_flag;
  void (*code_ptr) (struct Comp *);
  void *result;
  int arity;
  int num_bound;
  struct Comp *arg;
};

typedef struct Comp Comp;

void push_int(int val);
void push_func(void (*code_ptr)(Comp *), int arity);
void bind_ops();

void int_add(Comp *c);
void int_sub(Comp *c);
void int_mul(Comp *c);
void int_div(Comp *c);

#endif

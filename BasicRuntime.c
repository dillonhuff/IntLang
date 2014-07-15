#include <stdio.h>
#include <stdlib.h>

#include "BasicRuntime.h"

#define DEBUG_MODE 1

#define EVALUATED 1
#define NOT_EVALUATED 0

void print_stack();

int num_allocated_ptrs = 0;

void print_comp(Comp *c) {
  /*  printf("pc\n");
  if (c == NULL) {
    printf("NULL COMP\n");
  }
  printf("arity = %d\n", c->arity);
  printf("fd\n");*/
  printf("{ EVALED : %d, code_ptr : %ld, ", c->eval_flag, (long) c->code_ptr);
  //  printf("noo\n");
  if (c->eval_flag == EVALUATED) {
    printf("Result : %d, ", *((int*)c->result));
  }
  printf("Arity : %d, Num Bound : %d }\n", c->arity, c->num_bound);
}

struct Stack_Node {
  Comp *c;
  struct Stack_Node *next;
};

typedef struct Stack_Node Stack_Node;

// Memory management functions

void *alloc_mem(size_t size_to_alloc) {
  num_allocated_ptrs++;
  void *to_ret = malloc(size_to_alloc);
  return to_ret;
}

void *free_mem(void *ptr) {
  num_allocated_ptrs--;
  free(ptr);
  return;
}

// Comp creation, manipulation and destruction functions

Comp *copy_comp(Comp *c) {
  Comp *c_copy = alloc_mem(sizeof(Comp));
  c_copy->eval_flag = c->eval_flag;
  c_copy->code_ptr = c->code_ptr;
  c_copy->result = c->result;
  c_copy->arity = c->arity;
  c_copy->num_bound = c->num_bound;
  c_copy->arg = c->arg;
  return c_copy;
}

Comp *int_comp(int val) {
  //  print_stack();
  Comp *c = alloc_mem(sizeof(Comp));
  c->eval_flag = EVALUATED;
  c->code_ptr = NULL;
  int *res = alloc_mem(sizeof(int));
  *res = val;
  c->result = res;
  c->arity = 0;
  c->num_bound = 0;
  c->arg = NULL;
  return c;
}

Comp *func_comp(void (*code_ptr)(Comp *arg_list), int arity) {
  Comp *c = alloc_mem(sizeof(Comp));
  c->eval_flag = NOT_EVALUATED;
  c->code_ptr = code_ptr;
  c->result = NULL;
  c->arity = arity;
  c->num_bound = 0;
  c->arg = (Comp **) alloc_mem(arity*sizeof(Comp*));
  return c;
}

void add_arg(Comp *c, Comp *new_arg) {
  if (c->num_bound == c->arity) {
    printf("ERROR: Attempt to add argument to computation that is already full\n");
    return;
  }
  c->arg[c->num_bound] = new_arg;
  c->num_bound++;
  return;
}

void evaluate_comp(Comp *c) {
  printf("about to execute\n");
  c->code_ptr(c);
  c->eval_flag = EVALUATED;
  return;
}

Comp *nth_arg(Comp *c, int arg_num) {
  if (c->arity < arg_num) {
    printf("ERROR: Trying to get argument %d of function with arity %d\n", arg_num, c->arity);
  }
  return c->arg[arg_num - 1];
}

// Stack manipulation functions

Stack_Node *top = NULL;
int stack_size = 0;

void print_stack() {
  /*  if (top == NULL || *top == NULL) {
    printf("STACK IS NULL\n");
    return;
    }*/
  Stack_Node *node = top;
  while (node != NULL) {
    //    printf("In loop node->next == NULL ? %d\n", node->next == NULL);
    if (node->c == NULL) {
      printf("ERROR: Stack node has null comp\n");
    }
    //    printf("about to print comp\n");
    print_comp(node->c);
    node = node->next;
  }
  printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
  return;
}

void push_stack(Comp *c) {

  #if DEBUG_MODE
  //  printf("BEFORE PUSH\n");
  //  print_comp((*top)->c);
  //  print_stack();
  #endif

  Stack_Node *nn = (Stack_Node *) alloc_mem(sizeof(Stack_Node));
  nn->c = copy_comp(c);
  if (top == NULL) {
    nn->next = NULL;
  } else {
    Stack_Node *old_top = top;
    nn->next = old_top;
  }
  top = nn;
  stack_size++;

  #if DEBUG_MODE
  print_stack();
  #endif

  return;
}

Comp *pop_stack() {
  if (top == NULL) {
    printf("ERROR: Popping from empty stack\n");
    return (Comp *) -1;
  }

  #if DEBUG_MODE
  //  printf("Popping off of stack\n");
  #endif

  Stack_Node *cur_top = top;
  Comp *top_comp = cur_top->c;
  Stack_Node *next_top = cur_top->next;
  top = next_top;
  stack_size--;

  #if DEBUG_MODE
  print_stack();
  #endif
  return top_comp;
}

void push_int(int val) {
  printf("Pushing int\n");
  Comp *i_comp = int_comp(val);
  push_stack(i_comp);
  return;
}

void push_func(void (*code_ptr)(Comp *c), int arity) {
  printf("Pusing func\n");
  Comp *f_comp = func_comp(code_ptr, arity);
  push_stack(f_comp);
  return;
}

void bind_ops() {
  Comp *first = pop_stack();
  Comp *second = pop_stack();

  #if DEBUG_MODE
  //  printf("Binding together two comps\n");
  //  print_comp(first);
  //  print_comp(second);
  //  printf("\n");
  #endif
  printf("adding arg\n");
  add_arg(first, second);
  printf("done adding arg\n");
  if (first->arity == first->num_bound) {
    evaluate_comp(first);
  } else {
    push_stack(first);
  }
  printf("done with bind\n");
  return;
}

// Builtin functions

// Integer arithmetic
void int_add(Comp *c) {
  Comp *arg1 = nth_arg(c, 1);
  Comp *arg2 = nth_arg(c, 2);
  int *a1_ptr = (int *) arg1->result;
  int *a2_ptr = (int *) arg2->result;
  int sum = *a1_ptr + *a2_ptr;
  push_int(sum);
  return;
}

void int_sub(Comp *c) {
  Comp *arg1 = nth_arg(c, 1);
  Comp *arg2 = nth_arg(c, 2);
  int *a1_ptr = (int *) arg1->result;
  int *a2_ptr = (int *) arg2->result;
  int diff = *a1_ptr - *a2_ptr;
  push_int(diff);
  return;
}

void int_mul(Comp *c) {
  Comp *arg1 = nth_arg(c, 1);
  Comp *arg2 = nth_arg(c, 2);
  int *a1_ptr = (int *) arg1->result;
  int *a2_ptr = (int *) arg2->result;
  int prod = *a1_ptr * *a2_ptr;
  push_int(prod);
  return;
}

void int_div(Comp *c) {
  printf("DIVIDING\n");
  Comp *arg1 = nth_arg(c, 1);
  Comp *arg2 = nth_arg(c, 2);
  int *a1_ptr = (int *) arg1->result;
  int *a2_ptr = (int *) arg2->result;
  int quot = *a1_ptr / *a2_ptr;
  push_int(quot);
  return;
}

// Boolean connectives
void and(Comp *c) {
  Comp *arg1 = nth_arg(c, 1);
  Comp *arg2 = nth_arg(c, 2);
  int *a_ptr = (int*) arg1->result;
  int *b_ptr = (int*) arg2->result;
  push_int(*a_ptr && *b_ptr);
  return;
}

//Driver function
int main() {
  mfunc();
  return 0;
}

#include <stdint.h>
#include <stdio.h>

typedef struct PtrMapEntry {
  struct PtrMapEntry *prev;
  uint32_t key;
  uint32_t reg_usage;
  int32_t *stack_usage; // 0 terminated
} PtrMapEntry_t;

extern PtrMapEntry_t **GCINITHEAD;

static int32_t *TOPSTACK = 0;
static int32_t *BOTSTACK = 0xffffffff;

void walk_stack_usage(int32_t *stack_usage);

void update_top_stack(int32_t *newtop)
{
  TOPSTACK = (newtop > TOPSTACK) ? newtop : TOPSTACK;
}

void update_bot_stack(int32_t *newbot)
{
  BOTSTACK = (newbot < BOTSTACK) ? newbot : BOTSTACK;
}

void gc_init(void)
{
  PtrMapEntry_t *head = *GCINITHEAD;

  uint32_t count = 1;

  while (head) {
    printf("key = 0x%x\n", head->key);
    printf("reg = 0x%x\n", head->reg_usage);
    walk_stack_usage(head->stack_usage);
    head = head->prev;
    count++;
  }
}

void walk_stack_usage(int32_t *stack_usage)
{
  uint32_t idx = 0;
  while (stack_usage[idx]) {
    printf("Stack[%d] = %d\n", idx, stack_usage[idx]);
    idx++;
  }
}

void dump_stack_limits(void)
{
  printf("TOPSTACK = 0x%x\n", TOPSTACK);
  printf("BOTSTACK = 0x%x\n", BOTSTACK);
}

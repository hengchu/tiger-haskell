#include <stdint.h>
#include <stdio.h>

typedef struct PtrMapEntry {
  struct PtrMapEntry *prev;
  uint32_t key;
  uint32_t reg_usage;
  uint32_t *stack_usage; // 0 terminated
} PtrMapEntry_t;

extern PtrMapEntry_t **GCINITHEAD;

void gc_init(void)
{
  PtrMapEntry_t *head = *GCINITHEAD;

  do {
    printf("key = %x\n", head->key);
    head = head->prev;
  } while (head->prev);
}

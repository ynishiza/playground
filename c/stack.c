#include <stdlib.h>
#include "stack.h"

#define STACK_SIZE 200
#define NAME "yui"
static char stack[STACK_SIZE];
int stack_index = 0;

void stack_push(char c)
{
	if (stack_index > STACK_SIZE) stack_index = 0;
	stack[stack_index] = c;
	stack_index++;
}

char stack_pop()
{
	if (stack_index == 0) return NULL;
	stack_index--;
	return stack[stack_index];
}

dummy() {}

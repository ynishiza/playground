#include <stdio.h>
#include "test_function.h"
#include "../lib/helpers.h"
#include "../stack.h"

int increment();

static void testRegister(register int y);
static void testBlock();
static void testInitialization();
static void testStack();

extern int stack_index;

void testFunction() {
	printHeader("Increments");
	printInt(increment());
	printInt(increment());
	printInt(increment());

	printHeader("getchar");
	printf("Buffer size: %d\n", GETC_BUFFER_SIZE);
	printChar(getch());
	printChar(getch());
	ungetch('a');
	ungetch('b');
	printChar(getch());
	printChar(getch());
	printChar(getch());
	printChar(getch());

	printHeader("register");
	testRegister(10);

	printHeader("block");
	testBlock();

	printHeader("Initialization");
	testInitialization();
}

void testInitialization() {
	int x = 0;
	int y[] = { 1, 2, 3 };
	char z[] = "Hello";
	printInt(x);
	printInt(y[0]);
	printString(z);
}

void testRegister(register int y) {
	register int z = 1;
	printInt(z);
	printInt(y);
}

void testBlock() {
	int x = 1;
	int y = 0;
	printInt(y);
	{
		int x = 2;
		y += x;
		printInt(y);
	}
	y += x;
	printInt(y);
}

int increment() {
	static int x = 1;
	return x++;
}

void testStack()
{
	dummy();
	printHeader("Test stack");
	stack_push(1);
	stack_push(2);
	stack_push(3);
	printInt(stack_index);
	printInt(stack_pop());
	printInt(stack_pop());
	printInt(stack_pop());
	printInt(stack_pop());
	printInt(stack_pop());
	printInt(stack_pop());
}

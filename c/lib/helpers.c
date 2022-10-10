#include "helpers.h"
#include <string.h>

static char charBuffer[GETC_BUFFER_SIZE];
static int bufferPos = -1;

char getch() {
	if (bufferPos >= 0) {
		return charBuffer[bufferPos--];
	}
	return getchar();
}

void ungetch(char c) {
	bufferPos++;
	if (bufferPos == GETC_BUFFER_SIZE) {
		bufferPos = 0;
	}
	charBuffer[bufferPos] = c;
}


void printHeader(char *c)
{
	printf("=== %s ===\n", c);
}

void printChar(char c)
{
	printf("char: %c\n", c);
}

void printString(char *s)
{
	printf("string: %s\n", s);
}

void printManyStrings(char **s, int length) {
	for (int i = 0; i < length; i++) {
		printString(s[i]);
	}
}

void printInt(int i)
{
	printf("int: %d\n", i);
}

void printFloat(float f)
{
	printf("float: %f\n", f);
}

void printDouble(double d)
{
	printf("double: %f\n", d);
}

void printIntVariable(int *x) 
{
	printf("Integer Value:%d, Address:%p\n", *x, x);
}

void printAddress(void *x)
{
	printf("Address:%p\n", x);
}

void printCharVariable(char *c)
{
	printf("Char Value:%c, Address:%p\n", *c, c);
}

void printStringVariable(char *c) 
{
	const int l = strlen(c);
	printf("String Value:%s Length: %d  Start address:%p  End address: %p\n", c, l, c, c + l);
}

void printIntMatrix(int *m[], const int rows, const int cols) {
	printf("Matrix address %p\n", m);
	for (int i = 0; i < rows; i++) {
		printf("%p [%d] -> %p\t", m + i, i, m[i]);
		for (int j = 0; j < cols; j++) {
			printf("%p (%d,%d) = %d\t", m[i] + j, i, j, m[i][j]);
		}
		printf("\n");
	}
}

#include <stdio.h>
#include <stdlib.h>
#include "test_types.h"

static void testLiterals();
static void testSizeof();

static void incrementStaticVar();
static int htol2(char *c);

void testTypes() 
{
	/* testLiterals(); */
	testSizeof();
}

void testSizeof() {
	printHeader("sizeof");
	printf("sizeof int:%d\n", sizeof(int));
	printf("sizeof int*:%d\n", sizeof(int*));
	printf("sizeof void*:%d\n", sizeof(void*));
	printf("sizeof \"Hello\":%d\n", sizeof("Hello"));
	printf("sizeof 1:%d\n", sizeof(1));
	int arr[] = { 1, 2, 3};
	printf("sizeof array:%d\n", sizeof(arr));
	enum bool { T, F };
	printf("sizeof T:%d\n", sizeof(T));
}

void testLiterals() {
	printChar(353);
	printChar('a');
	printChar('\x61');
	printChar('\141');
	printInt(5.0 / 2);
	printInt(~2);
	printDouble(5.0 / 2);
	printDouble(5.0L);

	// htol
	printInt(htol2("0"));
	printInt(htol2("1"));
	printInt(htol2("a"));
	printInt(htol2("A"));
	printInt(htol2("f"));
	printInt(htol2("10"));

	printDouble(1e1);

	printHeader("Static variable test");
	incrementStaticVar();
	incrementStaticVar();
	incrementStaticVar();
}

int htol2(char *str) 
{
	int i = 0;
	int x = 0;
	while (str[i] != '\0')
	{
		char c = tolower(str[i]);
		int v;
		if (isdigit(c)) v = c - '0';
		else if (c >= 'a' && c <= 'f') v = c - 'a' + 10;
		else exit(EXIT_FAILURE);
		printf("%c %i\n", c, v);
		x = x * 16 + v;
		i++;
	}
	return x;
}

void incrementStaticVar() 
{
	static int x = 0;
	printInt(x++);
}

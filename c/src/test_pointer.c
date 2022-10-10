#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../lib/helpers.h"
#include "test_pointer.h"

#define MATRIXSIZE 3

static void testCopy();
static void testLeak();
static void testInitialization();
static void testArray();
static void testSort();
static void testAddresses();
static void testSwap();
static void testDoublePointer();
static void testVoid();
static void testMultidimensionalArray();
static void testFunctionPointer();

static void createLongString(char * str, int length);
static void strcpy2(char *s, char *t);

static void printNColumnMatrix(int m[][MATRIXSIZE], int rows);

static char * badstring();
static int map(void *[], int, void (*)(void *));

static void swapstrs(void *[], int, int);
static void qsort_strings(void *[], int length, int (*comp)(void *, void*));
static void _qsort_strings(void *[], int start, int end, int (*comp)(void *, void*));

static int reverse_strcmp(char *, char *);
static void toupper_str(char *);
static void tolower_str(char *);

void testPointer() {
	/* testArray(); */
	/* testInitialization(); */
	/* testCopy(); */
	/* testLeak(); */
	/* testSwap(); */
	/* testSort(); */
	/* testFunctionPointer(); */
	/* testDoublePointer(); */
	/* testVoid(); */
	/* testMultidimensionalArray(); */
}

void testAddresses() {
	const int x = 10;
	const long y = 1L;
	long *z;
	char c[10];

	z = &y;
	strcpy(c, "012345678");

	printf("\n%d\n%f\n", 010, 1.1);
	printIntVariable(&x);
	printIntVariable(z);
	printIntVariable(&y);

	printAddress(c);
	printAddress(&c);
	printAddress(&c[1]);
	printAddress(c + 1);


	printHeader("Address arithmetic");
	char i[10];
	double d[10];
	printAddress(d);
	printAddress(d + 1);
	printAddress(i);
	printAddress(i + 1);
}

void  testInitialization() {
	printHeader("Pointer initialization");
	char str1[] = "Hello";
	printStringVariable(str1);
	str1[0] = 'h';
	str1[1] = 'E';
	// Illegal
	/* str2[0] = 'h'; */
	printStringVariable(str1);

	// String
	char *str2 = (char[]){'a', 'b', 0};
	printChar(str2[0]);
	printChar(str2[1]);
	printStringVariable(str2);

	// Array
	int *i = (int[]) { 1, 2 };
	i[0] = 3;
	printInt(i[0]);

	// With malloc
	i = malloc(sizeof(int));
	*i = 10;
	printInt(*i);

}

void testArray() {
	/* int *i = 0; */
	printHeader("Array");
	int ints1[] = { 1, 2 };
	int ints2[] = { 3, 4 };
	int *intptr;
	intptr = ints2;

	printIntVariable(ints1);
	printIntVariable(intptr);
	printIntVariable(intptr + 1);
	printIntVariable(&intptr[0]);
	printIntVariable(&intptr[1]);

	// Pointer notation
	*ints1 = 10;
	*(ints1 + 1) = 20;
	printIntVariable(ints1);
	printIntVariable(ints1 + 1);
	

	printInt(ints1[0]);
	printInt(*ints1);
	printInt(ints1[1]);
	printInt(*(ints1 + 1));

	// Array notation
	int *intptr2 = malloc(10);
	intptr2[0] = 10;
	intptr2[1] = 20;
	printIntVariable(&intptr2[0]);
	printIntVariable(&intptr2[1]);
	printIntVariable(intptr2);
	printIntVariable(intptr2 + 1);

	printHeader("sizeof");
	printInt(sizeof(intptr));
	printInt(sizeof(char));

	printHeader("Useless string");
	printString(badstring());
}

void testLeak() {
	printHeader("Leak");
	char *cptr, *c3;
	cptr = malloc(1);
	c3 = malloc(1);
	createLongString(cptr, 1000);
	printStringVariable(cptr);
	printAddress(cptr + strlen(cptr));
	printStringVariable(c3);
}

void testCopy() {
	printHeader("String copy");
	char str1[] = "Hello";
	char *str2 = malloc(10);
	strcpy2(str2, str1);
	printStringVariable(str1);
	printStringVariable(str2);
}

void testSwap() {
	printHeader("Test swapstrs");
	char *strings[2] = {
		"AAA",
		"BBB",
		"CCC"
	};
	printString(strings[0]);
	printString(strings[1]);
	printString(strings[2]);
	swapstrs((void **) strings, 0, 1);
	swapstrs((void **) strings, 1, 2);
	printString(strings[0]);
	printString(strings[1]);
	printString(strings[2]);
}

void testSort() {
	printHeader("Test string sort");
	const int length = 12;
	char *strings[]= {
		"ABD",
		"ABBA",
		"ABBC",
		"ABB",
		"AC",
		"ABC",
		"E",
		"ABA",
		"ACD",
		"ABE",
		"D",
		"ABF",
	};

	printf("Before\n");
	printManyStrings(strings, length);
	printf("After\n");
	qsort_strings((void **) strings, length, (int (*)(void *, void*)) strcmp);
	printManyStrings(strings, length);

	printf("Reverse\n");
	qsort_strings((void **) strings, length, (int (*)(void *, void*)) reverse_strcmp);
	printManyStrings(strings, length);
}

void testDoublePointer() {
	printHeader("Test double pointer");
	const char *constantStrs[] = {
		"ABC",
		"DEF"
	};
	/* constantStrs[0][1] = 'a'; // ERROR */
	printChar(constantStrs[0][1]);
	printManyStrings(constantStrs, 2);

	char *strs[2];
	const char str1[] = "Hello";
	const char str2[] = "THere";
	strs[0] = str1;
	strs[1] = str2;
	strs[0][0] = 'a';
	printManyStrings(strs, 2);

	printHeader("Test parenthesis");
	int is1[] = {1,2};
	int is2[] = {3,4};
	int *ints1[2];
	int (*ints2)[2];
	int *(ints3[1]);
	ints1[0] = is1;
	ints1[1] = is2;

	ints2 = is1;

	ints3[0] = is1;
	ints3[1] = is2;
	printInt(ints1[0][0]);
	printInt(ints1[1][0]);

	printInt(ints2[0][0]);
	printInt(ints2[1][0]); // gibberish

	printInt(ints3[0][0]);
	printInt(ints3[1][0]);
}

void testMultidimensionalArray() {
	printHeader("Test multidimensional array initialization");
	int nrows = 3;
	int matrix[][MATRIXSIZE] = {
		{ 10, 20, 3 },
		{ 3, 4, 5 },
		{ 4, 5, 0 },
	};
	int *matrix2[nrows];
	matrix2[0] = malloc(sizeof(int) * MATRIXSIZE);
	matrix2[1] = malloc(sizeof(int) * MATRIXSIZE);
	matrix2[2] = matrix[0];
	matrix2[0][0] = 1;
	matrix2[0][1] = 1;

	printAddress(matrix[0]);
	printAddress(&matrix[0][1]);
	printAddress(&matrix[0][2]);
	printAddress(matrix[1]);
	printAddress(matrix[2]);
	printNColumnMatrix(matrix, 2);

	printAddress(matrix2);
	printAddress(matrix2[0]);
	printAddress(&matrix2[0][1]);
	printAddress(&matrix2[0][2]);
	printAddress(matrix2[1]);
	printNColumnMatrix( matrix2, nrows);
	printIntMatrix(matrix2, nrows, MATRIXSIZE);

	int **m3 = matrix;
	printAddress(m3);

	int *m4[] = {
		(int[]) { 1, 2 },
		(int[]) { 3, 4 },
	};
	printIntMatrix(m4, 2, 2);
}

void testVoid() {
	printHeader("Test void pointer");
	double d[] = { 1, 2, 3 };
	void *v = d;
	printAddress(v);
	printAddress(v + 1);
	printAddress(d);
	printAddress(d + 1);
	printDouble(*((double *)v));
	printDouble(*((double *)v + 1));
	/* printDouble((double) v[1]); */
}

void testFunctionPointer() {
	printHeader("Test function point");
	int length = 2;
	char *strings[2];
	strings[0] = malloc(10);
	strings[1] = malloc(10);
	strcpy(strings[0], "aBc");
	strcpy(strings[1], "dEf");
	printManyStrings(strings, length);
	map((void **)strings, 2, (void (*)(void *)) toupper_str);
	printManyStrings(strings, length);
	map((void **)strings, 2, (void (*)(void *)) tolower_str);
	printManyStrings(strings, length);
}

void printNColumnMatrix(int m[][MATRIXSIZE], int rows) {
	printf("Matrix address %p\n", m);
	for (int i = 0; i < rows; i++) {
		printf("%p [%d]-> %p\t\t", m + i, i, m[i]);
		for (int j = 0; j < MATRIXSIZE; j++) {
			printf("%p (%d,%d) = %d\t", m[i] + j, i, j, m[i][j]);
		}
		printf("\n");
	}
}

void createLongString(char * str, int length) {
	int i = 0;
	for (; i < length; i++) {
		str[i] = i == 0 ? 1 : (i + 1) % 255;
	}
	str[length] = '\0';
}

void strcpy2(char *s, char *t) {
	while ((*s++ = *t++) != '\0')
		;
}

char *badstring() {
	char str[] = "Hello";
	printString(str);
	return str;
}

void qsort_strings(void *values[], int length, int (*comp)(void *, void*)) {
	_qsort_strings(values, 0, length - 1, comp);
}

void _qsort_strings(void *values[], int start, int end,int (*comp)(void *, void*)) {
	if (start >= end) return;

	// Choose a random pivot
	int pivotIndex = (start + end)/2;
	void *pivotValue = values[pivotIndex]; 

	/* printf("start: %d end: %d pivot: %c\n", start, end, *pivotValue); */

	// Move pivot out of the way
	swapstrs(values, start, pivotIndex);

	int current = start;
	for (int i = current + 1; i <= end; i++) {
		if ((*comp)(pivotValue, values[i]) >= 0) {
			swapstrs(values, ++current, i);
		}
	}

	// Put pivot in right place.
	swapstrs(values, current, start);

	// Sort rest.
	_qsort_strings(values, start, current - 1, comp);
	_qsort_strings(values, current + 1, end, comp);
}

void swapstrs(void *values[], int i, int j) {
	if (i == j) return;
	void *v = values[i];
	values[i] = values[j];
	values[j] = v;
}

void toupper_str(char *str) {
	int l = strlen(str);
	for (int i = 0; i < l; i++) {
		str[i] = toupper(str[i]);
	}
}

void tolower_str(char *str) {
	int l = strlen(str);
	for (int i = 0; i < l; i++) {
		str[i] = tolower(str[i]);
	}
}

int reverse_strcmp(char *x, char *y) {
	return strcmp(y, x);
}

int map(void *value[], int length, void (*fn)(void *)) {
	for (int i = 0; i < length; i++)
		(*fn)(value[i]);
}

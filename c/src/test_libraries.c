#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "../lib/helpers.h"

union number {
	int i;
	double d;
	float f;
};

static void testInputOutput();
static void testPrintf();
static void testVariableArguments();
static void testScanf();
static void testMemory();
static void testMem();
static void testFile();
static void testString();

static void printManyInts(int,...);
static void printFormats(char **, int, char *, union number);

void testLibraries() {
	/* testInputOutput(); */
	/* testVariableArguments(); */
	/* testMem(); */
	/* testMemory(); */
	/* testPrintf(); */
	/* testFile(); */
	testString();
}

void testInputOutput() {
	char c;
	printf("Type $ to end");
	do {
		c = getchar();
		putchar(toupper(c));
	} while (c != '$' && c != EOF);
}

void testPrintf() {
	int i;
	char expr[10];
	union number value;
	printHeader("printf");
	printf("int:%d hex:%03x octal:%o\n", 10, 10, 10);
	char *intFormats[] = {
		"d",
		"3d",
		".3d",
		"03d",
		"-3d",
		"+3d",
	};
	value.i = 1;
	printFormats(intFormats, sizeof(intFormats) / sizeof(char*), "int", value);

	char *floatFormats[] = {
		"f",
		".3f",
		".10f",
		"2f",
		"20f",
		"020f",
		"20.3f"
	};
	value.d = 12345.12345;
	printFormats(floatFormats, sizeof(floatFormats) / sizeof(char*), "double", value);

	printHeader("sprintf");
	char buffer[100];
	sprintf(buffer, "int:%d", 10);
	printString(buffer);
}

void printFormats(char **formats, int length, char *value_type, union number value) {
	for (int i = 0; i < length; i++) {
		char expr[20];
		sprintf(expr, "%s\t%%%s\n", formats[i], formats[i]);

		if (value_type == "int") {
			printf(expr, value.i);
		} else if (value_type == "double") {
			printf(expr, value.d);
		} else if (value_type == "float") {
			printf(expr, value.f);
		}
	}
}

void testVariableArguments() {
	printHeader("variable arguments");
	int values[] = {
		1, 2, 3,
	};
	printManyInts(2, values, values + 1);
}

void testMemory() {
	printHeader("memory");
	int length = 100;
	char *c1 = malloc(length);
	char *c2 = calloc(length, sizeof(char));
	/* c1[length - 1] = 0; */
	/* c2[length - 1] = 0; */
	for (int i = 0; i < length; i++) {
		printf("(%d,%d) ", c1[i], c2[i]);
	}

	c1 = realloc(c1, 1000);
	free(c1);
	free(c2);
	/* free(c1); */
}

void testScanf() {
	printHeader("Test scanf");
	int scanned;
	int i;
	short int si;
	unsigned char uc;
	long l;
	long long ll;
	float f;
	double d;
	long double ld;
	char c;
	char string[100];

	sscanf("1", "%d", &i);
	sscanf("1", "%ld", &l);
	sscanf("1", "%Ld", &ll);
	sscanf("-1", "%hd", &si);
	sscanf("-1", "%hhd", &uc);
	sscanf("1.1", "%f", &f);
	sscanf("1.1", "%lf", &d);
	sscanf("1.1", "%Lf", &ld);

	sscanf("0xa", "%i", &i);
	sscanf("0Xa", "%i", &i);
	sscanf("010", "%i", &i);

	sscanf("10", "%o", &i);
	sscanf("a", "%x", &i);
	sscanf("a", "%X", &i);

	sscanf("Hello there", "%c", &c);
	sscanf("Hello there", "%s", string);
	sscanf("Hello there", "%[a-zA-Z0-9 ]", string);
	sscanf("Hello there", "%[^tl]", string);

	sscanf("  a9", "%d", &i);
}

void testMem() {
	printHeader("mem functions");
	char c[20];
	int i[20];
	memset(c, 1, 5);
	memset(c, 1, 500);
	memset(i, 0, 15);
	void *v = i;
	((char*)v)[0] = 1;
	((char*)v)[1] = 1;
	((char*)v)[2] = 1;

	// memcpy
	int j[20], k[] = { 1, 2, 3 };
	memcpy(j, k, 3 * sizeof(int));
	memcpy(j + 1, j, 3 * sizeof(int));
	// memcpy(j, k, 30 * sizeof(int));  BAD


	// memcmp
	int cmpresult;
	char c1[] = { 1, 2, 3 }, c2[] = { 1, 2, 2 };
	cmpresult = memcmp(c1, c2, 2);
	cmpresult = memcmp(c1, c2, 3);
	cmpresult = memcmp(c2, c1, 3);

	// memmove
}

void testFile() {
	char buffer[100];
	printHeader("Test file");
	char path[] = "/tmp/test";
	FILE *file;
	file = fopen(path, "w+");
	fputc('a', file);
	fputc('b', file);
	fprintf(file, "This is a test");
	fflush(file);

	FILE *copyFile = fopen(path, "r");
	char c;
	while (1) {
		c = fgetc(copyFile);
		printf("pos: %li\t char: %c\n", ftell(copyFile), c);
		if (c == EOF) break;
	} 

	// Seek
	FILE *seekTest = fopen("/tmp/seekTest", "w+");
	fprintf(seekTest, "This is a test");
	fseek(seekTest, 0, SEEK_SET);
	fseek(seekTest, 2, SEEK_SET);
	fseek(seekTest, 3, SEEK_CUR);
	fseek(seekTest, -2, SEEK_CUR);
	fseek(seekTest, 0, SEEK_END);
	fseek(seekTest, -1, SEEK_END);
	fseek(seekTest, 1, SEEK_END);


	// read/write
	FILE *rwTest = fopen("/tmp/rwtest", "w+");
	fpos_t pos;
	fgetpos(rwTest, &pos);
	fwrite("Hello", sizeof(char), 6, rwTest);
	fwrite((int[]) { 1, 2, 3 }, sizeof(int), 3, rwTest);
	fwrite((char[]) { 4, 5 }, sizeof(char), 3, rwTest);
	fflush(rwTest);
	fsetpos(rwTest, &pos);
	fread(buffer, sizeof(char), 6, rwTest);

	// gets,puts
	FILE *lineTest = fopen("/tmp/lineTest", "w+");
	fputs("This\n", lineTest);
	fputs("is\n", lineTest);
	fputs("a test\n", lineTest);
	fflush(lineTest);
	rewind(lineTest);
	fgets(buffer, 100, lineTest);
	fgets(buffer, 100, lineTest);
	fgets(buffer, 100, lineTest);

	// error
	if (fopen("/tmp/randomfile", "r") == NULL) {
		printString("Failed to open file");
	}

	// stdout/stderr
	fprintf(stdout, "stdout: Hello\n");
	fprintf(stderr, "stderr: Hello\n");
	fclose(stdout);

	// Redirect
	freopen("redirect.txt", "a", stdout);
	freopen("redirect.txt", "a", stderr);
	printf("stdout: Hello after reopen\n");
	fprintf(stderr, "stderr: Hello after reopen\n");
	fclose(stdout);
	fclose(stderr);
}

void testString() {
	printHeader("Test String");
	char *buffer = calloc(100, sizeof(char));
	strcat(buffer, "Hello");
	strncat(buffer, "Hello", 1);

	memset(buffer, 1, 20);
	strcpy(buffer, "This");
	strncpy(buffer, "Bye", 3);
	strncpy(buffer, "Bye", 5);

	char text[] = "Hello";
	char search[10];
	char *ptr;
	ptr = (char *) strchr(text, 'e');
	ptr = (char *) strchr(text, 'k');
	ptr = strstr(text, "el");
	strcpy(search, "abcde");
	ptr = strpbrk(text, search);
	ptr = strpbrk(text, "ab");


	system("echo Test");
}

void printManyInts(int count, ...) {
	va_list vaptr;
	va_start(vaptr, &count);
	for (int i = 0; i < count; i++) {
		int *x = va_arg(vaptr, int*);
		printf("i=%d\tvalue=%d\n", i, *x);
	}
	va_end(vaptr);
}

#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include "./src/test_libraries.h"
#include "lib/helpers.h"
#include "./src/test_function.h"
#include "./src/test_pointer.h"
#include "./src/test_io.h"

extern int xyz;
int isdigit(int c);
int tolower(int c);
void *malloc(size_t n);


// Comment
int main(int argc, char**argv) 
{
	printf("Hello");
	for (int i = 0; i < argc; i++) {
		printf("argument %d: %s\n", i, argv[i]);
	}

	/* testPointer(); */
	/* testTypes(); */
	/* testStructure(); */
	/* testIO(); */
	testLibraries();
}



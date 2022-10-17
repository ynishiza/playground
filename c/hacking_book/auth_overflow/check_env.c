#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
	char *name = argv[1];
	char *value = getenv(name);
	char c;
	long int diff = &c - value;
	printf("Variable %s is at %p\n", name, value);
	/* printf("Local variable is at %p. Difference is %ld\n", &c, &c - value); */
	printf("Local variable is at %p. Difference is %ld\n", &c, diff);
}

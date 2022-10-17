#include <stdio.h>
#include <stdlib.h>

void printAll(char *c, int length) {
	for (int i = 0; i < length; i++) {
		putc(c[i], stdout);
	}
}

int main() {
	char *str = malloc(200);
	char c;
	int i = 0;
	do {
		/* c = getc(stdin); */
		/* if (c == EOF) break; */
		/* str[i] = c; */
		int count = fread(str + i, 1, 1, stdin);
		if (count == 0) break;
		i++;
	} while (1);

	printf("length: %d\n", i);
	printAll(str, i);
}


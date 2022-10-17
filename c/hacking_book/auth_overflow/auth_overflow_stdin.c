#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int auth_check(char *password, int length) {
	char buffer[16];
	char result3[20];
	result3[0] = 0;
	printf("local variable: %p\n", buffer);
	memcpy(buffer, password, length);

	if (strcmp(buffer, "Yui") == 0) result3[0] = 1;
	return result3[0];
}

int main() {
	char buffer[200];
	int l = sizeof(buffer);
	int count = fread(buffer, 1, l, stdin);
	printf("Variable EXPLOIT at %p\n", getenv("EXPLOIT"));
	printf("Password length: %d\n", count);
	if (auth_check(buffer, count) == 1) {
		printf("Access authorized\n");
	} else {
		printf("Access denied\n");
	}
}

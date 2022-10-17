#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int auth_check(char *password) {
	char buffer[16];
	char result3[20];
	result3[0] = 0;
	strcpy(buffer, password);

	if (strcmp(buffer, "Yui") == 0) result3[0] = 1;
	return result3[0];
}

int main(int argc, char *argv[]) {
	if (argc < 2) {
		printf("Provide password\n");
		exit(1);
	}
	printf("%s\n", argv[1]);
	if (auth_check(argv[1]) == 1) {
		printf("Access authorized\n");
	} else {
		printf("Access denied\n");
	}
}

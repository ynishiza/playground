#include <time.h>
#include <stdio.h>
void dump_value(void *, int);

int main() {
	struct tm now;
	long s = time(0);
	localtime_r(&s, &now);
	printf("Time since epoch %ld\n", s);
	printf("Time %d:%02d:%02d\n", now.tm_hour, now.tm_min, now.tm_sec);

	printf("Dumping time %p\n", &now);
	dump_value(&now, sizeof(now));
	
	long long t;
	printf("%lf", 1.0f);
}

void dump_value(void *value, int size) {
	unsigned char * unit = (unsigned char*) value;
	for (int i = 0; i < size; i++) {
		if (i > 0 && i % 8 == 0) printf("\n");
		printf("0x%02x ", unit[i]);
	}
	printf("\n");
}



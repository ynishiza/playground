void test_function2() {
	char buffer[10];
	buffer[0] = 'A';
}

void test_function(int a, int b, int c, int d) {
	int flag;
	char buffer[10];
	flag = 1234;
	buffer[0] = 'A';
	buffer[1] = 'B';
	buffer[2] = 'C';
	test_function2();
}

int main () {
	test_function(1, 2, 3, 4);
}

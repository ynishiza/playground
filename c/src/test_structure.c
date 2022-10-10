#include <stdio.h>
#include <stdlib.h>
#include "test_structure.h"
#include "../lib/helpers.h"

static void testInitialization();
static void testFunction();
static void testPointer();
static void testTree();
static void testUnion();
static void testBitflag();

typedef char* String;
typedef int (*comparer)(void *, void*);
struct point {
	int x;
	int y;
};

struct rect {
	struct point bottomLeft;
	struct point topRight;
};

// Binary search tree
struct bst {
	void *value;
	comparer compare;
	struct bst *left;
	struct bst *right;
};

union number {
	int i;
	float f;
	double d;
};

static void printPoint(struct point);
static struct point addPoints(struct point, struct point);
static void printRect(struct rect);
static void incremeentPoint(struct point*);

static void sillyMutation(struct rect);

static void addnode(struct bst*, void *);
static void traverse(struct bst*, void (*)(struct bst*));

static struct bst *bstalloc();
static struct bst *create_intnode(int *);
static int intcmp(int *, int *);
static void print_intnode(struct bst*);

void testStructure() {
	/* testInitialization(); */
	/* testFunction(); */
	/* testPointer(); */
	testTree();
	/* testUnion(); */
	/* testBitflag(); */
}

void testInitialization() {
	printHeader("Initialization");
	struct point p1 = { 1, 2 };
	printPoint(p1);
	p1.x = 3;
	printPoint(p1);

	struct {
		double x;
		double y;
	} pd1;
	pd1.x = 10;

	printDouble(pd1.x);
	printDouble(pd1.y);
	printf("size of point:%d\tsize of rect:%d\n", sizeof(struct point), sizeof(struct rect));

	// Array of structs
	struct point pts[] = {
		{ 1, 2 },
		{ 3, 4 },
		5, 6,
	};
	int ptsLen = sizeof pts/sizeof(struct point);
	printPoint(pts[0]);
	printPoint(pts[2]);
	printf("sizeof array: %d\nsizeof point: %d\nlength: %d\n", sizeof pts, sizeof(struct point), ptsLen);

}

void testFunction() {
	printHeader("Function");
	struct point p1 = { 1, 2 }, p2 = { 3, 4 };
	struct rect r1 = { p1, p2 };

	// Copy
	struct point p3;
	p3 = p1;
	p3.x = -100;
	printPoint(p1);
	printPoint(p3);

	// Silly increment, since can't mutate
	printRect(r1);
	sillyMutation(r1);
	printRect(r1);

	//
	printPoint(p1);
	incremeentPoint(&p1);
	printPoint(p1);
}

void testPointer() {
	printHeader("Test Pointer");
	struct point p;
	struct point *pp;
	pp = &p;

	pp->x = 10;
	printPoint(*pp);
	printPoint(p);
	printInt(pp->x);
	printInt((*pp).x);
}

void testUnion() {
	printHeader("Union");
	printInt(sizeof(union number));
	printInt(sizeof(double));

	union number value = { 1 };
	printInt(value.i);
	printDouble(value.d);
	printFloat(value.f);
	value.d = 10.0;
	printInt(value.i);
	printDouble(value.d);
	printFloat(value.f);
	value.f = -100.0;
	printInt(value.i);
	printDouble(value.d);
	printFloat(value.f);
	value.i = -10;
	printInt(value.i);
	printDouble(value.d);
	printFloat(value.f);

	printHeader("Union pointer");
	union number *ptr = malloc(sizeof(union number));
	ptr->i = 10;
	printInt(ptr->i);
	printDouble(ptr->d);
	printFloat(ptr->f);
	ptr->d = -10;
	printInt(ptr->i);
	printDouble(ptr->d);
	printFloat(ptr->f);
}

void testTree() {
	printHeader("Test tree");
	int values[] = {
		10, 2, 3, 1, 20, 15
	};
	int length = sizeof(values) / sizeof(int);

	struct bst *root = create_intnode(values);
	for (int i = 1; i < length; i++) {
		printf("Add value: %d\n", values[i]);
		addnode(root, values + i);
	}
	traverse(root, (void (*)(struct bst*)) print_intnode);
}

void testBitflag() {
	printHeader("Bit flag");
	struct {
		unsigned int f1: 1;
		unsigned int f2: 2;
		int f3: 1;
		int f4: 2;
	} flags;
	printInt(flags.f1);
	printInt(flags.f2);
	for (int i = 0; i < 10; i++) {
		flags.f1 = flags.f2 = flags.f3 = flags.f4 = i;
		printf("i:%d\tunsigned, width 1:%d\tunsigned, width 2:%d\tsigned, width 1:%d\t signed, width 2:%d\n", i, flags.f1, flags.f2, flags.f3, flags.f4);
	}
}

void printPoint(struct point p) {
	printf("x:%d y:%d\n", p.x, p.y);
}

void printRect(struct rect r) {
	printPoint(r.bottomLeft);
	printPoint(r.topRight);
}

struct point addPoints(struct point p1, struct point p2) {
	p1.x += p2.x;
	p1.y += p2.y;
	return p1;
}

void sillyMutation(struct rect r) {
	struct point p;
	r.bottomLeft.x = 2;
	r.topRight = p;
}

void incremeentPoint(struct point *p) {
	p->x++;
	p->y++;
}



/* *********** Start BST ********** */
void addnode(struct bst* node, void *value) {
	int result = (*(node->compare))(node->value, value);
	struct bst* next = NULL;

	if (result <= 0 && node->left != NULL) {
		next = node->left;
	} else if (result > 0 && node->right != NULL) {
		next = node->right;
	}

	if (next != NULL) {
		addnode(next, value);
		return;
	}

	struct bst *newnode = bstalloc();
	newnode->value = value;
	newnode->compare = node->compare;
	if (result <= 0) node->left = newnode;
	else node->right = newnode;
}

struct bst *bstalloc() {
	struct bst* node = malloc(sizeof(struct bst));
	node->left = NULL;
	node->right = NULL;
	node->value = NULL;
	node->compare = NULL;
	return node;
}

void traverse(struct bst* node, void (*fn)(struct bst*)) {
	if (node == NULL) return;
	traverse(node->left, fn);
	(*fn)(node);
	traverse(node->right, fn);
}

struct bst *create_intnode(int *value) {
	struct bst *node = bstalloc();
	node->compare = (comparer) intcmp;
	node->value = value;
	return node;
}

void print_intnode(struct bst* node) {
	printf("node: %p\tleft: %p\tright: %p\tvalue: %d\n", node, node->left, node->right, *((int *) node->value));
}

int intcmp(int *v1, int *v2) {
	return *v1 - *v2;
}
/* *********** End BST ********** */

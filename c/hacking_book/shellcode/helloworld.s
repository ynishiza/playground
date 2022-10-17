BITS 32
call mark_below
db "Hello, world!", 0x0a, 0x0d

mark_below:
	pop ecx 
	mov eax, 4
	mov ebx, 1
	mov edx, 15
	int 0x80

	mov eax, 1
	mov ebx, 0
	int 0x80

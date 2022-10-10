#!/bin/bash
ENVNAME=myenv

printUsage() {
	echo "Usage
\$ source \"$ENVNAME/bin/activate\"
\$ ansible --version
\
"
}

if [[ -d "$ENVNAME" ]]
then
	echo "virtual environment $ENVNAME exists."
	printUsage
	exit 1
fi

virtualenv "$ENVNAME"
# shellcheck disable=SC1091
source "$ENVNAME/bin/activate"
pip install ansible==2.9

printUsage

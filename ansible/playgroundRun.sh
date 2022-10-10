#!/bin/bash
OPTIONS=
OPTIONS=-v
# OPTIONS=-vvv
ansible-playbook $OPTIONS -i ./playgroundHosts.ini  ./playground.yml

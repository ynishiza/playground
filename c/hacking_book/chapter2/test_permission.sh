#!/bin/bash
printf "Effective uid: %s\n" $(id -u)
printf "Real uid: %s\n" $(id -ru)

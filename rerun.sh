#!/bin/bash

python3 tangle.py $1.md && racket -i -e "(enter! \"$1.mk.rkt\")"


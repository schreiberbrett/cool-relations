#!/bin/bash

python3 build.py
racket -i -e "(enter! \"tangled.rkt\")"
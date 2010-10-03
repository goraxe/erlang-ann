#!/bin/bash
set -e
make
escript main.beam xor.ann

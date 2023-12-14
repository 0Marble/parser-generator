#! /bin/bash

cargo build --release --bin wordgen
valgrind --tool=callgrind --callgrind-out-file=cg.out --collect-jumps=yes --simulate-cache=yes ../target/release/wordgen

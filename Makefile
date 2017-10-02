all: deps cs

deps:
	chicken-install anaphora combinatorics format test

cs: crib-square.ss main.ss
	csc main.ss -o $@

test: crib-square.test.ss crib-square.ss
	csi $^

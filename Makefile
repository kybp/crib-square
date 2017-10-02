all: deps cs

deps:
	chicken-install anaphora combinatorics format test

cs: main.ss
	csc $^ -o $@

test: crib-square.test.ss crib-square.ss
	csi $^

.PHONY: default test selftest

default: test

test: selftest

selftest:
	bash bin/sicp

.PHONY: default test selftest compile-scheme

default: test

test: selftest

selftest:
	bash bin/sicp

compile-scheme:
	bash bin/get-scheme.sh

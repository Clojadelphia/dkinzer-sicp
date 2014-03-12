.PHONY: default test selftest compile-scheme build

default: test

test: selftest

selftest:
	bash bin/run.sh

compile-scheme:
	bash bin/get-scheme.sh

build:
	bash bin/build.sh

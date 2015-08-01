.PHONY: test
PROJECT = mkr

include resources/make/common.mk

test:
	make check-unit

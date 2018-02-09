TEST_SUITE = JSON-Schema-Test-Suite
TEST_SUITE_DEST = spec/$(TEST_SUITE)
TEST_SUITE_URL = https://github.com/json-schema-org/$(TEST_SUITE)

.PHONY: all test

all:
ifeq ($(wildcard $(TEST_SUITE_DEST)),)
	git clone $(TEST_SUITE_URL) $(TEST_SUITE_DEST)
endif

test: all
	resty t/test.lua

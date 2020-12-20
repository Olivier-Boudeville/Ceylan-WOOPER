WOOPER_TOP = ..

.PHONY: all test-local prepare-otp-test


# Helper modules (ex: from Myriad like basic_utils.beam, or directly from WOOPER
# like wooper_class_manager.beam, etc.) are not listed here as dependencies,
# their possible lacking will be detected at runtime.


# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test


prepare-otp-test:
	@cd $(WOOPER_TOP) && $(MAKE) -s rebar3-create-app-file


include $(WOOPER_TOP)/GNUmakesettings.inc

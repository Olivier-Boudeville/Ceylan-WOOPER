WOOPER_TOP = ..

.PHONY: all test test-local prepare-otp-test info-test-options

MODULES_DIRS = interfaces


# Helper modules (e.g. from Myriad like basic_utils.beam, or directly from
# WOOPER like wooper_class_manager.beam, etc.) are not listed here as
# dependencies, their possible lacking will be detected at runtime.


# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test


prepare-otp-test:
	@cd $(WOOPER_TOP) && $(MAKE) -s create-app-file


info-test-options:
	@echo "ERL_PARAMETERIZED_LAUNCHER = $(ERL_PARAMETERIZED_LAUNCHER)"
	@echo "EXEC_INTERNAL_OPTIONS = $(EXEC_INTERNAL_OPTIONS)"
	@echo "EXEC_EXTERNAL_OPTIONS = $(EXEC_EXTERNAL_OPTIONS)"
	@echo "CMD_LINE_OPT = $(CMD_LINE_OPT)"


include $(WOOPER_TOP)/GNUmakesettings.inc

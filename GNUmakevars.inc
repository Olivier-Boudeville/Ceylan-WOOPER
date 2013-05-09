# Prerequisite: WOOPER_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = WOOPER
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 1.0
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = wooper
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(WOOPER_TOP)
endif


WOOPER_RELEASE_BASE := wooper-$(PROJECT_VERSION)

WOOPER_RELEASE_ARCHIVE_ZIP := $(WOOPER_RELEASE_BASE).zip
WOOPER_RELEASE_ARCHIVE_BZ2 := $(WOOPER_RELEASE_BASE).tar.bz2


# Source section.
WOOPER_SRC  := $(WOOPER_TOP)/src

# BEAM path section.
WOOPER_BEAM  := $(WOOPER_TOP)/src

# Include path section.
WOOPER_INC = -I$(WOOPER_SRC)


DOC_ROOT       = $(WOOPER_TOP)/../../../doc
WOOPER_DOC_DIR = $(DOC_ROOT)/web/main/documentation/wooper


INC += $(WOOPER_INC)



ifndef VM_TEST_NAME

VM_NAME := wooper_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := wooper_test

endif


BEAM_DIRS += $(WOOPER_BEAM)


# COMMON_TOP may be a symbolic link pointing to the actual Common package to be
# used:
COMMON_TOP = $(WOOPER_TOP)/../common


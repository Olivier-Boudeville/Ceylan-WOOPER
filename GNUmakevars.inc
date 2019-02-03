# Prerequisite: WOOPER_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = WOOPER
endif


WOOPER_VERSION := 1.0

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(WOOPER_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = Ceylan-WOOPER
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(WOOPER_TOP)
endif


WOOPER_RELEASE_BASE := wooper-$(PROJECT_VERSION)

WOOPER_RELEASE_ARCHIVE_ZIP := $(WOOPER_RELEASE_BASE).zip
WOOPER_RELEASE_ARCHIVE_BZ2 := $(WOOPER_RELEASE_BASE).tar.bz2
WOOPER_RELEASE_ARCHIVE_XZ  := $(WOOPER_RELEASE_BASE).tar.xz


# Source section.
WOOPER_SRC := $(WOOPER_TOP)/src

# BEAM path section.
WOOPER_BEAM := $(WOOPER_TOP)

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




# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
WOOPER_BEAM_DIRS = $(WOOPER_BEAM)/src      \
				   $(WOOPER_BEAM)/examples


# We rely on Myriad as well:
BEAM_DIRS += $(WOOPER_BEAM_DIRS)


# Overall settings section.


ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_DEBUG=true


else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_DEBUG=false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# By default, this debug mode is enabled:
ifndef ENABLE_DEBUG

	ENABLE_DEBUG = true

endif



# Tells whether the debug mode will be activated for the next WOOPER classes to
# be built (maximum performance versus extended checkings).
ifeq ($(ENABLE_DEBUG),true)

	ENABLE_WOOPER_DEBUG_OPT = -Dwooper_debug

else

	ENABLE_WOOPER_DEBUG_OPT =

endif


# Compiler section.


# Modules needed for the bootstrapping for others (hence to be built first, and
# not parse-transformed; typically *used* by parse-transforms, or being a
# parse-transform themselves)
#
# (see the 'Bootstrap section' in GNUmakerules-explicit.inc for their
# special-cased build)
#
ifndef BOOTSTRAP_MODULES

	# We want to bootstrap all relevant WOOPER modules (Myriad expected to be
	# already fully built)


	# We list here all the WOOPER-level prerequisites of the WOOPER
	# parse-transform:
	#
	# - wooper_info: to manage class-level information

	BOOTSTRAP_MODULES = $(WOOPER_TOP)/src/wooper_info.beam

endif


ifndef COMPILER_OPT_FOR_WOOPER_CLASSES

	   COMPILER_OPT_FOR_WOOPER_CLASSES =                      \
			$(ERLANG_COMPILER_OPT_BASE)                       \
			$(ENABLE_WOOPER_DEBUG_OPT)                        \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_WOOPER)

endif


ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	   # Standard (non-class) modules in WOOPER shall not be compiled with the
	   #  WOOPER parse-transform, but with the Myriad one:
	   #
	   ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD)

endif


# Conditionally defined so that upper layer may update these settings (ex:
# adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
			$(ERLANG_COMPILER_OPT_BASE)                                 \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES) \
			$(ENABLE_WOOPER_DEBUG_OPT)

endif


## Parse-transform related section.


# List here all the directories expected to contain parse transforms:
#
# (to be overridden by each upper layer)
#
ifndef PARSE_TRANSFORM_DIRS

	   PARSE_TRANSFORM_DIRS = $(WOOPER_TOP)/src

endif


# So that the (compilation of the) WOOPER parse transform can benefit from the
# Myriad parse transform:
#
ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD) \
							 $(ENABLE_WOOPER_DEBUG_OPT)


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the WOOPER one:
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_WOOPER =                 \
					$(OVERALL_PZ_OPT)                            \
					-pz $(WOOPER_TOP)/src                        \
					'+{parse_transform,wooper_parse_transform}'


# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that will call directly, by
# themselves, at the Erlang level, this 'WOOPER' parse transform):
#
ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT

	   ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_WOOPER)

endif


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the WOOPER one:
#
COMPILER_PARSE_TRANSFORM_FOR_WOOPER_CLASSES_OPT = \
					'+{parse_transform,wooper_parse_transform}'


# For later reuse in the next layer of the software stack:
WOOPER_PLT_FILE := $(WOOPER_TOP)/wooper.plt


# This is the merged PLT of the level just below in the software stack.
# For 'WOOPER', it is the PLT of 'Myriad':
ifndef PREDECESSOR_PLT
	   PREDECESSOR_PLT = $(MYRIAD_PLT_FILE)
endif


# We define the path to the Myriad layer:
#
# (it may be a symbolic link pointing to the actual Myriad package to be used,
# which itself may be either a 'Ceylan-Myriad' directory or a 'myriad' one)
#
ifneq ($(wildcard $(WOOPER_TOP)/../myriad),)

	MYRIAD_TOP = $(WOOPER_TOP)/../myriad

else

	# Default:
	MYRIAD_TOP = $(WOOPER_TOP)/../Ceylan-Myriad

endif

# Prerequisite: WOOPER_TOP must be set.


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(WOOPER_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-WOOPER
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := wooper
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
WOOPER_VERSION := 2.0.9


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(WOOPER_VERSION)
endif


# Variable name intentionally not including the current layer, for more generic
# rules:

ifndef VERSION_FOR_REBAR3
	VERSION_FOR_REBAR3 := $(PROJECT_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = $(PROJECT_NAME)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(WOOPER_TOP)
endif



# For any quick, local, non-packaged update thereof:
MYRIAD_SIBLING_BUILD = $(LAYER_TOP)/../Ceylan-Myriad


# The OTP tree of the Myriad application within a local OTP _build tree:
MYRIAD_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/myriad

# Where Myriad BEAMs are to lie when used as an OTP application:
MYRIAD_LOCAL_EBIN := $(MYRIAD_LOCAL_APP)/ebin/



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


# In an OTP/rebar3-style application layout, at compilation time, libraries
# making use of Myriad will expect its includes to be located in:
#
MYRIAD_OTP_INC := -I$(WOOPER_TOP)/../myriad/include/


INC += $(WOOPER_INC) $(MYRIAD_OTP_INC)



# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
WOOPER_BEAM_DIRS = $(WOOPER_BEAM)/src      \
				   $(WOOPER_BEAM)/examples


# For OTP releases and all:

WOOPER_REBAR_BUILD_BASE = $(REBAR_BUILD_DIR)/lib/wooper


# When building WOOPER in an OTP context from its usual, GIT root (ex: with
# 'make rebar3-application'), the BEAMs of Myriad are to be found in the OTP
# build tree - rather than in a supposedly fully-built usual root for Myriad:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls should no prior directory correspond among in the BEAM paths).
#
MYRIAD_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(MYRIAD_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout, at compilation time as well,
# modules compiled (directly or not) by the Myriad parse transform will expect
# its module to be located in:
#
MYRIAD_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../myriad/ebin/


# To locate Myriad modules from all OTP contexts:
MYRIAD_PARSE_TRANSFORMS_PZ_OPT = \
	-pz $(MYRIAD_OTP_BEAM_DIR_FROM_USUAL) \
	-pz $(MYRIAD_OTP_BEAM_DIR_FROM_OTP)


# We rely on Myriad as well:
BEAM_DIRS += $(WOOPER_BEAM_DIRS)


DOC_ROOT       = $(WOOPER_TOP)/../../../doc
WOOPER_DOC_DIR = $(DOC_ROOT)/web/main/documentation/wooper


ifndef VM_TEST_NAME

VM_NAME := wooper_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := wooper_test

endif


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

	ENABLE_WOOPER_DEBUG_OPT = -Dwooper_debug_mode

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

	BOOTSTRAP_MODULES = $(WOOPER_SRC)/wooper_info.beam

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
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD) \
				$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)

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
					$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)            \
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
WOOPER_PLT_FILE := $(WOOPER_TOP)/Ceylan-WOOPER.plt


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

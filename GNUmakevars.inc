# Prerequisite: WOOPER_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid WOOPER execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# Tells whether the debug mode will be activated for the next WOOPER classes to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)
	ENABLE_WOOPER_DEBUG_OPT = -Dwooper_debug_mode
else
	ENABLE_WOOPER_DEBUG_OPT =
endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - construction: for the creation of instances
#  - destruction: for the deletion of instances
#  - embodiment: to delegate the hosting of an instance to an existing process
#  - serialisation: for the serialisation of instances
#  - deserialisation: for the deserialisation of instances
#  - hot_update: for the hot-code update of classes
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_construction
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_destruction
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_embodiment
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_serialisation
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_deserialisation
	#WOOPER_DEBUG_FLAGS += -Dwooper_debug_hot_update
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	WOOPER_CHECK_FLAGS += -Dwooper_check_construction
	WOOPER_CHECK_FLAGS += -Dwooper_check_destruction
	WOOPER_CHECK_FLAGS += -Dwooper_check_embodiment
	WOOPER_CHECK_FLAGS += -Dwooper_check_serialisation
	WOOPER_CHECK_FLAGS += -Dwooper_check_deserialisation
	WOOPER_CHECK_FLAGS += -Dwooper_check_hot_update
endif



# As, with WOOPER, traces still rely on console output, the default here is to
# ellipse emitted traces of lesser severity; uncomment next line to disable any
# ellipsing of them (in order to have all traces in full):
#
# (note that, for such a make variable to be assigned by an upper layer and to
# be taken into account here, WOOPER must be built from an overall, integrated
# Make context - not as a series of separate builds)
#
#WOOPER_UNELLIPSED_TRACES_OPT = -Dwooper_unellipsed_traces



# One may specify wooper_unellipsed_traces if not wanting the emitted traces to
# be ellipsed (typically if relying on an advanced logger able to cope with
# longer traces, such as Ceylan-Traces).

ERLANG_COMPILER_OPT_BASE += -Dwooper_version=\"$(WOOPER_VERSION)\" \
							$(WOOPER_DEBUG_FLAGS)                  \
							$(WOOPER_CHECK_FLAGS)                  \
							$(WOOPER_UNELLIPSED_TRACES_OPT)



# Useful to target for example the root of the current layer (e.g. to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(WOOPER_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-WOOPER
endif


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := wooper
endif


# PROJECT_NAME_FOR_OTP should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_OTP
	PROJECT_NAME_FOR_OTP := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see wooper_utils:get_wooper_version{_string,}/0):
#
WOOPER_VERSION := 2.0.32


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(WOOPER_VERSION)
endif



ifndef PACKAGE_TOP
	PACKAGE_TOP := $(WOOPER_TOP)
endif


# For any quick, local, non release-packaged update thereof:
MYRIAD_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/myriad
MYRIAD_SIBLING_BUILD = $(LAYER_TOP)/../myriad


# The OTP tree of the Myriad application within a local OTP _build tree:
MYRIAD_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/myriad

# Where Myriad BEAMs are to lie when used as an OTP application:
MYRIAD_LOCAL_EBIN := $(MYRIAD_LOCAL_APP)/ebin



WOOPER_RELEASE_BASE := wooper-$(PROJECT_VERSION)

WOOPER_RELEASE_ARCHIVE_ZIP := $(WOOPER_RELEASE_BASE).zip
WOOPER_RELEASE_ARCHIVE_BZ2 := $(WOOPER_RELEASE_BASE).tar.bz2
WOOPER_RELEASE_ARCHIVE_XZ  := $(WOOPER_RELEASE_BASE).tar.xz


# Source section.
WOOPER_SRC := $(WOOPER_TOP)/src

# BEAM path section.
WOOPER_BEAM_DIR := $(WOOPER_SRC)

WOOPER_INTERFACES_DIR := $(WOOPER_SRC)/interfaces


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
WOOPER_BEAM_DIRS := $(WOOPER_BEAM_DIR) $(WOOPER_INTERFACES_DIR) $(WOOPER_TOP)/..


INC += -I$(WOOPER_SRC)/interfaces


# For OTP releases and all:

WOOPER_REBAR_BUILD_BASE = $(MYRIAD_REBAR_BUILD_DIR)/lib/wooper


# When building WOOPER in an OTP context from its usual (Git) root (e.g. with
# 'make rebar3-application'), the BEAMs of Myriad (the layer just below it) are
# to be found in the OTP build tree - rather than in a supposedly fully-built
# usual root for Myriad:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls, should no prior directory correspond in the BEAM paths).
#
MYRIAD_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(MYRIAD_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout (not speaking of sibling base
# directories here), at compilation time as well, modules compiled (directly or
# not) by the Myriad parse transform will expect its modules to be located in:
#
# (a third place could be considered, in the _build tree of Myriad itself, yet
# it would not provide any additional chance)
#
MYRIAD_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../myriad/ebin/


# To locate Myriad modules from all OTP contexts:
MYRIAD_PARSE_TRANSFORMS_PZ_OPT = -pz $(MYRIAD_OTP_BEAM_DIR_FROM_USUAL) \
								 -pz $(MYRIAD_OTP_BEAM_DIR_FROM_OTP)


# We rely on Myriad as well:
BEAM_DIRS += $(WOOPER_BEAM_DIRS)


ifndef VM_TEST_NAME
	VM_NAME := wooper_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := wooper_test
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

	# ENABLE_WOOPER_DEBUG_OPT managed transversely:
	COMPILER_OPT_FOR_WOOPER_CLASSES =                     \
		$(ERLANG_COMPILER_OPT_BASE)                       \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_WOOPER) \
		$(ERLANG_COMPILER_EXTRA_OPTS)

endif


ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	# Standard (non-class) modules in WOOPER shall not be compiled with the
	# WOOPER parse-transform, but with the Myriad one:
	#
	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD)      \
		$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)

endif


ERLANG_COMPILER_EXTRA_OPTS += $(ENABLE_WOOPER_DEBUG_OPT)



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
# (to find wooper_parse_transform.beam, both the WOOPER 'src' and 'ebin'
# directories are specified: the former for our native build, the latter for a
# rebar3-based one)
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_WOOPER = \
	$(OVERALL_PA_OPTS)                           \
	-pz $(WOOPER_TOP)/ebin                       \
	-pz $(WOOPER_TOP)/src                        \
	$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)            \
	'+{parse_transform,wooper_parse_transform}'


# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that they will call directly,
# by themselves, at the Erlang level, this 'WOOPER' parse transform):
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
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(MYRIAD_PLT_FILE)
endif



# Documentation section.

# No $(LAYER_TOP) in includes, it would be misleading as edoc is run from the
# clone root, not from doc:
#
ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {macros,[{$(PROJECT_EDOC_VERSION_KEY),"$(PROJECT_VERSION)"}]}, {includes,["..", "$(MYRIAD_TOP)/.."]}, {overview,"doc/overview.edoc"}]'
endif



# We define the path to the Myriad layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(WOOPER_TOP)/_checkouts/myriad/.),)
	MYRIAD_TOP = $(WOOPER_TOP)/_checkouts/myriad
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(WOOPER_TOP)/../myriad/.),)
		MYRIAD_TOP = $(WOOPER_TOP)/../myriad
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(WOOPER_TOP)/_build/default/lib/myriad/.),)
			MYRIAD_TOP = $(WOOPER_TOP)/_build/default/lib/myriad
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Myriad from '$(realpath .)'.)
		endif
	endif
endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(MYRIAD_TOP)
endif

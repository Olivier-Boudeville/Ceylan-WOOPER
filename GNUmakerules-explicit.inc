# Here are gathered explicit (immediate, static) rules for WOOPER.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: WOOPER_TOP must be set.


.PHONY: default-wooper-rule clean-ast-debug clean-ast-debug-wooper


# The first non-generic, therefore default, rule:
default-wooper-rule: all


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-wooper

clean-ast-debug-wooper:
	@find . -name 'WOOPER-*put-AST*.txt' -exec /bin/rm '{}' ';'



# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should be built prior to
# any parse-transform (here, the WOOPER one) using them, and the resulting BEAM
# must be produced in the right directory (not at the root).

# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.


# We still need a special build here, so that the BEAM is generated in src
# rather than at the root of WOOPER:
#
$(WOOPER_TOP)/src/wooper_info.beam: $(WOOPER_TOP)/src/wooper_info.erl
	@echo "     Compiling bootstrap module $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o src $<

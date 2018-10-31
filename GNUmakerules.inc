# Add all WOOPER-specific rules below.


# Outside prerequisites: $(WOOPER_SRC)/wooper.hrl wooper_class_manager and
# modules from Ceylan-Myriad.


# Allows any upper layer to override as a whole the WOOPER rules used to compile
# classes and other modules:
#
ifndef OVERRIDE_WOOPER_BUILD_RULES


# This first section is for actual modules (class_*_test.erl), not classes,
# hence no WOOPER-level parse transform is involved:

class_%_test.beam: class_%_test.erl class_%_test.hrl
	@echo "		Compiling WOOPER test with header $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


# Most frequently-used rule for tests:
class_%_test.beam: class_%_test.erl
	@echo "		Compiling WOOPER test $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


# This second section handles the classes:

class_%.beam: class_%.erl class_%.hrl class_%_dependencies
	@echo "		Compiling WOOPER class with header $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<


class_%.beam: class_%.erl class_%_dependencies
	@echo "		Compiling WOOPER class $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<


class_%.beam: class_%.erl class_%.hrl
	@echo "		Compiling WOOPER no-dependency class with header $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<


# Most frequently-used rule for classes:
class_%.beam: class_%.erl
	@echo "		Compiling WOOPER no-dependency class $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_WOOPER_CLASSES) -o $@ $<


# Allows for example 'make:files([class_XXX]).' or 'make:all().':
# (deactivated, otherwise could be the default rule applied)
#Emakefile:
#	@echo "     Generating Emakefile"
#	@echo "{'*',[debug_info,{i,\"$(WOOPER_INC)\"}]}." > Emakefile

endif


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-wooper

clean-ast-debug-wooper:
	@find . -name 'WOOPER-*put-AST*.txt' -exec srm '{}' ';'


# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should be built prior to
# any parse-transform (here, the WOOPER one) using them, and the resulting BEAM
# must be produced in the right directory (not at the root).

# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.

# No need for a special build here:
#$(WOOPER_TOP)/src/wooper_info.beam: $(WOOPER_TOP)/src/wooper_info.erl

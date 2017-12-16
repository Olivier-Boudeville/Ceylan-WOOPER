WOOPER_TOP = ..

DOCUTILS_TOP = $(WOOPER_TOP)

OVERALL_DOCUMENT_TARGET = wooper.rst

DOC_GENERATED_FILES = wooper.pdf wooper.html

DOC_FILES = $(DOC_GENERATED_FILES) wooper-example.png


# The current, operational version of WOOPER:
CURRENT_VERSION := 1.0

CURRENT_WOOPER_DOC := wooper-$(CURRENT_VERSION).pdf

PROJECT_CSS := wooper.css

WEB_CONTENT = *.html *.css *.png *.pdf

WEB_MIRROR :=

WEB_MIRROR_ROOT :=


.PHONY: full-doc full-doc-wooper-current export-doc

all:

doc:

full-doc:
	@$(MAKE) $(DOC_GENERATED_FILES) VIEW_PDF=no


full-doc-wooper-current:
	@$(MAKE) $(CURRENT_WOOPER_DOC)



export-doc: full-doc export-to-official export-to-mirror


export-to-official:
	@echo "   Exporting WOOPER documentation to official website"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/WOOPER/


export-to-mirror:
	@echo "   Exporting WOOPER documentation to mirror website"
	@/bin/scp $(WEB_CONTENT) $(WEB_MIRROR):$(WEB_MIRROR_ROOT)


clean-doc:
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_WOOPER_DOC)


# For COMMON_TOP:
include $(WOOPER_TOP)/GNUmakesettings.inc

# Not used, for a better readability:
#PROJECT_CSS = $(COMMON_TOP)/doc/Ceylan-docutils.css

# Now included in the common general rules:
#include $(COMMON_TOP)/doc/GNUmakerules-docutils.inc

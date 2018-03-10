WOOPER_TOP = ..

DOCUTILS_TOP = $(WOOPER_TOP)

OVERALL_DOCUMENT_TARGET = wooper.rst

DOC_GENERATED_FILES = wooper.pdf wooper.html

DOC_FILES = $(DOC_GENERATED_FILES) wooper-example.png


# The current, operational version of WOOPER:
CURRENT_VERSION := 1.0

CURRENT_WOOPER_DOC := wooper-$(CURRENT_VERSION).pdf

PROJECT_CSS := pygments-default.css,wooper.css


WEB_CONTENT = *.html *.css *.png wooper.pdf


.PHONY: all do full-doc clone-mirror                     \
		export-doc export-to-official export-to-mirror   \
		info-web clean clean-doc

all:

doc:

full-doc:
	@$(MAKE) $(DOC_GENERATED_FILES) VIEW_PDF=no


# Create a separate repository for the GitHub pages branch:
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-WOOPER.git $(shell basename $(WEB_MIRROR_BRANCH)) && git checkout gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


export-to-official:
	@echo "   Exporting WOOPER documentation to official website"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/WOOPER/


export-to-mirror:
	@echo "   Exporting WOOPER documentation to mirror website in $(WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(WEB_MIRROR_BRANCH) && cd $(WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "WOOPER doc updated." && git push


info-web:
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_MIRROR_BRANCH = $(WEB_MIRROR_BRANCH)"


clean: clean-doc

clean-doc:
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_WOOPER_DOC) $(DOC_GENERATED_FILES)


# For COMMON_TOP:
include $(WOOPER_TOP)/GNUmakesettings.inc

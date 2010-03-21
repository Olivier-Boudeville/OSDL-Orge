ORGE_TOP = ..

include $(ORGE_TOP)/GNUmakesettings.inc

DOCUTILS_TOP = $(ORGE_TOP)

OVERALL_DOCUMENT_SOURCE = Orge-game-system.rst


# List of all modules having documentation to be built:
MODULES_DOCUMENT_DIRS = $(TEXTS_BASE_DIR)


.PHONY: doc generate-html-doc install install-html-doc 


doc: generate-html-doc

generate-html-doc: generate-html-main-doc generate-html-modules-doc

generate-html-main-doc:

generate-html-modules-doc:
	@echo "  Generating HTML documentation from following modules: $(MODULES_DOCUMENT_DIRS)"  
	@for m in $(MODULES_DOCUMENT_DIRS) ; do ( cd $$m/doc ; $(MAKE) -s generate-html-doc ) ; done



install: install-html-doc


# Installs the Orge generated documentation in OSDL web tree:	
# (no 'INSTALL_DOC_TARGET=$(ORGE_WEB_INSTALL_DIR)' is specified in the
# '$(MAKE)' command-line as ORGE_WEB_INSTALL_DIR is based on ORGE_TOP
# and therefore the doc location would change depending on the depth of the
# doc module)
install-html-doc: generate-html-doc
	@echo "  Copying Orge documentation from following modules: $(MODULES_DOCUMENT_DIRS), targeting $(ORGE_WEB_INSTALL_DIR)"  
	@/bin/cp -f -L Orge.css $(ORGE_WEB_INSTALL_DIR)
	@for m in $(MODULES_DOCUMENT_DIRS) ; do ( cd $$m/doc ; $(MAKE) -s install-html-doc ) ; done





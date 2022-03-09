ORGE_TOP = ../../../../..


include $(ORGE_TOP)/GNUmakesettings.inc

DOCUTILS_TOP = .

DOC_HTML_CONTENT := Orge-languages-management.html
DOC_PDF_CONTENT  := Orge-languages-management.pdf

DOC_CONTENT      := $(DOC_HTML_CONTENT) $(DOC_PDF_CONTENT) Orge.css

TEXT_WEB_INSTALL_DIR = $(ORGE_WEB_INSTALL_DIR)/texts


# First (default) target, to avoid that building Orge results in a PDF being
# generated and even displayed:
doc:


# Includes the PDF, as it is stored and referenced in the website:
generate-html-doc: $(DOC_CONTENT)



install-html-doc: generate-html-doc
	@echo "  Installing $(DOC_CONTENT) in $(TEXT_WEB_INSTALL_DIR)"
	@mkdir -p $(TEXT_WEB_INSTALL_DIR)
	@/bin/cp -f $(DOC_CONTENT) $(TEXT_WEB_INSTALL_DIR)



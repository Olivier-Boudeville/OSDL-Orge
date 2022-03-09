ORGE_TOP = ../../..

DOC_BASE_NAME := Orge-servers


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_ORGE_SRV_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_ORGE_SRV_HTML)


CURRENT_ORGE_SRV_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_ORGE_SRV_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



# PROJECT_CSS centralised.

#PROJECT_ICON := orge-icon.png


# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(ORGE_WEB_MIRROR_BRANCH)")



.PHONY: all doc html pdf create-links clone-mirror                     \
		export-doc export-doc-to-official export-doc-to-mirror         \
		info info-web



# Default do-nothing target:
all:
	@echo "  Run 'make doc' to generate the documentation of the OSDL-Orge servers."


doc: html pdf create-api-doc


html: local-html

pdf: local-pdf


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_ORGE_SRV_HTML) index.html



# Creates a separate repository for the GitHub pages branch:
clone-mirror:
	@echo "  Creating a separate repository for website in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/OSDL-Orge $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


export-doc: clean-doc doc export-doc-to-official export-doc-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/WOOPER/
#
export-doc-to-official: clean-doc create-links doc
	@echo "   Exporting OSDL-Orge documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) -r $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/OSDL-Orge/


export-doc-to-mirror: clean-doc create-links doc
	@echo "   Exporting OSDL-Orge documentation to mirror website in $(ORGE_WEB_MIRROR_BRANCH)"
	@/bin/cp -rf $(WEB_CONTENT) $(ORGE_WEB_MIRROR_BRANCH) && cd $(ORGE_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "OSDL-Orge doc updated." && git push && git pull --ff-only



info: info-doc info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "ORGE_WEB_MIRROR_BRANCH = $(ORGE_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For WOOPER_TOP and MYRIAD_TOP:
include $(ORGE_TOP)/GNUmakesettings.inc

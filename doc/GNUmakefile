ORGE_TOP = ..


.PHONY: full-doc create-doc regenerate-doc make-doc-repository \
	view-doc to-print \
	clean clean-doc-repository info-local

	
OVERALL_DOCUMENT_SOURCE = Orge-game-system.rst

	
include $(ORGE_TOP)/GNUmakerules.inc



# Documentation section.


# Centralization of documentation sources is necessary, as the LateX generator
# from docutils will not find images in directories otherwise.
full-doc: create-doc view-doc


create-doc: regenerate-doc clean-doc-repository make-doc-repository
	@echo "     Generating full (PDF) documentation from $(OVERALL_DOCUMENT_SOURCE) in $(TMP_RST_REPOSITORY)"
	@$(MAKE) $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET)
	

regenerate-doc:
	@echo "     Regenerating all documentation elements"
	@cd $(ORGE_TOP) && $(MAKE) -s doc
	

make-doc-repository:
	@echo "     Copying documentation sources to $(TMP_RST_REPOSITORY)"
	@mkdir -p $(TMP_RST_REPOSITORY)
	@find $(ORGE_TOP) -name $(TMP_RST_REPOSITORY) -prune -o \( -name '*.rst' -o -name '*.png' \) -exec cp -f '{}' $(TMP_RST_REPOSITORY) ';'


view-doc:
	@$(PDF_VIEWER) $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET) 1>/dev/null 2>&1
	

to-print: create-doc
	@echo "    Transferring document $(OVERALL_DOCUMENT_TARGET) to $(PRINT_SERVER)"
	@scp $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_TARGET) $(PRINT_LOCATION) 
		
		
clean: clean-doc-repository


clean-doc-repository:
	@echo "     Cleaning documentation repository in $(TMP_RST_REPOSITORY)"
	-@rm -rf $(TMP_RST_REPOSITORY)
			

info: info-local


info-local:
	@echo "OVERALL_DOCUMENT_SOURCE = $(OVERALL_DOCUMENT_SOURCE)"
	@echo "OVERALL_DOCUMENT_TARGET = $(OVERALL_DOCUMENT_TARGET)"
	@echo "PRINT_SERVER = $(PRINT_SERVER)"

	

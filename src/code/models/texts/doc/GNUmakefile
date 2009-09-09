ORGE_TOP = ../../../../..


DOC_CONTENT = Orge-languages-management.html


generate-html-doc: $(DOC_CONTENT)


install-html-doc: $(DOC_CONTENT)
	@#echo "  Installing $< in $(ORGE_WEB_INSTALL_DIR)"
	@echo "  Installing $<"
	@mkdir -p $(ORGE_WEB_INSTALL_DIR)
	@/bin/cp -f $< $(ORGE_WEB_INSTALL_DIR)
	
	
include $(ORGE_TOP)/GNUmakerules.inc


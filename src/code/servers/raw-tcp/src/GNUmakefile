ORGE_TOP = ../../../../..

install-hook install-prod-hook:
	@if [ -z "$(INSTALLATION_PREFIX)" ] ; then echo "Error, no installation prefix set." 1>&2 ; exit 10 ; else $(MAKE) generate-start-script ; fi
	@$(MAKE) clean-bin


generate-start-script:
	@echo "  Generating start-orge-server.sh in $(INSTALLATION_PREFIX)/orge/bin"
	@mkdir -p "$(INSTALLATION_PREFIX)/orge/bin"
	@/bin/cat start-orge-server.sh-template|sed 's|ST_EPMD_PORT|$(EPMD_PORT)|g'|sed 's|ST_TCP_LOW_RANGE|$(TCP_FIREWALL_LOW)|g'|sed 's|ST_TCP_HIGH_RANGE|$(TCP_FIREWALL_HIGH)|g' > "$(INSTALLATION_PREFIX)/orge/bin/start-orge-server.sh"
	@chmod +x "$(INSTALLATION_PREFIX)/orge/bin/start-orge-server.sh"


# Script not be used there:
clean-bin:
	@/bin/rm -f "$(INSTALLATION_PREFIX)/orge/bin/start-orge-server-from-sources.sh"


info-install-script:
	@echo "INSTALLATION_PREFIX = $(INSTALLATION_PREFIX)"


include $(ORGE_TOP)/GNUmakerules.inc


ORGE_TOP = ../../../..


install install-prod: generate-monitor-script

generate-monitor-script:
	@echo "  Generating start-orge-monitor.sh in $(INSTALLATION_PREFIX)/orge/bin"
	@mkdir -p "$(INSTALLATION_PREFIX)/orge/bin"
	@/bin/cat start-orge-monitor.sh-template|sed 's|ST_EPMD_PORT|$(EPMD_PORT)|g' > "$(INSTALLATION_PREFIX)/orge/bin/start-monitor-server.sh"
	@chmod +x "$(INSTALLATION_PREFIX)/orge/bin/start-monitor-server.sh"

include $(ORGE_TOP)/GNUmakerules.inc


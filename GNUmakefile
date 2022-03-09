ORGE_TOP = .

.PHONY: bz2 work-archive to-archive info-local info-tools


MODULES_DIRS = doc src test tools

# No trace supervisor or graphical output wanted when running all tests from the
# root (batch mode vs interactive one):
#
CMD_LINE_OPT="--batch"


all:
	@echo "   Building all, in parallel over $(CORE_COUNT) core(s), from "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! (if [ -d $$m ]; then cd $$m && \
	$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd ..; else echo "     (directory $$m skipped)"; \
	fi); then exit 1; fi; done


include $(ORGE_TOP)/GNUmakesettings.inc


# Archive section.


bz2: clean
	@echo "    Making an archive of the whole Orge trunk \
	(except local archives and SVN directories)"
	mkdir -p $(ARCHIVE_ROOT) && cd $(ORGE_TOP)/../.. && \
	tar cvjf $(ARCHIVE_FILE) --exclude .svn Orge/trunk
	@echo && echo "Orge full archive stored in $(ARCHIVE_FILE)"


work-archive: clean
	@echo "    Making a work archive of the Orge framework \
	(except external documentation, but including SVN)"
	mkdir -p $(ARCHIVE_ROOT) && cd $(ORGE_TOP)/../.. && \
	tar cvjf $(WORK_ARCHIVE_FILE) Orge/trunk
	@echo && echo "Orge full work archive stored in $(WORK_ARCHIVE_FILE)"


to-archive: bz2
	@echo "    Transferring archive $(ARCHIVE_FILE) to $(ARCHIVE_SERVER)"
	@scp $(ARCHIVE_FILE) $(ARCHIVE_LOCATION)


install: install-hook

install-prod: install-prod-hook


install-hook:
	@cd $(ORGE_TOP)/src/code/servers/raw-tcp/src && $(MAKE) install INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)"
	@cd $(ORGE_TOP)/src/code/servers/monitoring && $(MAKE) install INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)"

install-prod-hook:
	@cd $(ORGE_TOP)/src/code/servers/raw-tcp/src && $(MAKE) install-prod INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)"
	@cd $(ORGE_TOP)/src/code/servers/monitoring && $(MAKE) install-prod INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)"


info-local:
	@echo "FQDN = $(FQDN)"
	@echo "BEAM_PATHS = $(BEAM_PATHS)"
	@echo "ARCHIVE_LOCATION = $(ARCHIVE_LOCATION)"


info-tools:
	@echo "ERLANG_INTERPRETER = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_COMPILER = $(ERLANG_COMPILER)"
	@echo "DOT = $(DOT)"
	@echo "GNUPLOT = $(GNUPLOT)"
	@echo "IMG_VIEWER = $(IMG_VIEWER)"

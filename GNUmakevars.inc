# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Orge
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 0.3
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = orge
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(ORGE_TOP)
endif



# LOANI-repository root:
LOANI_REPOSITORY_ROOT = $(ORGE_TOP)/../../..

# LOANI-installations root:
LOANI_INSTALLATIONS_ROOT = $(LOANI_REPOSITORY_ROOT)/../LOANI-installations


OSDL_TARGET_VERSION := 0.5

ORGE_WEB_INSTALL_DIR = $(LOANI_INSTALLATIONS_ROOT)/OSDL-$(OSDL_TARGET_VERSION)/share/OSDL/doc/web/main/documentation/OSDL/Orge


# egeoip root (for IP geolocation):
EGEOIP_ROOT = $(LOANI_INSTALLATIONS_ROOT)/egeoip


# Previous setting was to use a full Ceylan install, now preferring using a
# dedicated Ceylan-Erlang install, except for documentation:

# Ceylan sources supposed to be available at the same directory level as OSDL:
CEYLAN_ROOT := $(LOANI_REPOSITORY_ROOT)/ceylan
CEYLAN_TRUNK := $(CEYLAN_ROOT)/Ceylan/trunk
#CEYLAN_ERLANG_TOP := $(CEYLAN_TRUNK)/src/code/scripts/erlang
CEYLAN_DOC_TOP    := $(CEYLAN_TRUNK)/src/doc

# Now preferring using a dedicated Ceylan-Erlang install:
CEYLAN_ERLANG_TOP := $(LOANI_REPOSITORY_ROOT)/Ceylan-Erlang
#CEYLAN_DOC_TOP    := $(CEYLAN_ERLANG_TOP)/common/doc


# Will define COMMON_TOP and WOOPER_TOP:
TRACES_TOP = $(CEYLAN_ERLANG_TOP)/traces


# Non-Orge OSDL:
OSDL_ROOT := $(ORGE_TOP)/../../OSDL/trunk/src/doc/web/main/documentation/OSDL/Orge


OSDL_TARGET_VERSION := 0.5



ORGE_DEV_TOP := $(ORGE_TOP)/src/code


# Must always be found from the current directory (thanks to symlink for ex):
PROJECT_CSS = pygments-default.css,orge.css



# Directory section.

CLIENTS_BASE_DIR := $(ORGE_DEV_TOP)/clients
MODELS_BASE_DIR  := $(ORGE_DEV_TOP)/models
RULESET_BASE_DIR := $(ORGE_DEV_TOP)/ruleset
SERVERS_BASE_DIR := $(ORGE_DEV_TOP)/servers

TCP_CLIENTS_BASE_DIR := $(CLIENTS_BASE_DIR)/raw-tcp
UDP_CLIENTS_BASE_DIR := $(CLIENTS_BASE_DIR)/raw-udp

CREATURES_BASE_DIR := $(MODELS_BASE_DIR)/creatures
OBJECTS_BASE_DIR   := $(MODELS_BASE_DIR)/objects
PLACES_BASE_DIR    := $(MODELS_BASE_DIR)/places
TEXTS_BASE_DIR     := $(MODELS_BASE_DIR)/texts

SOCIETY_BASE_DIR := $(RULESET_BASE_DIR)/society

TCP_SERVERS_BASE_DIR := $(SERVERS_BASE_DIR)/raw-tcp
UDP_SERVERS_BASE_DIR := $(SERVERS_BASE_DIR)/raw-udp

FUNCTIONAL_BASE_DIR  := $(SERVERS_BASE_DIR)/functional-services

CLIENT_MANAGEMENT_BASE_DIR := $(FUNCTIONAL_BASE_DIR)/client-management
DATABASE_STORAGE_BASE_DIR  := $(FUNCTIONAL_BASE_DIR)/database-storage


# Source section.

TCP_CLIENTS_SRC := $(TCP_CLIENTS_BASE_DIR)/src
UDP_CLIENTS_SRC := $(UDP_CLIENTS_BASE_DIR)/src

CREATURES_SRC := $(CREATURES_BASE_DIR)
OBJECTS_SRC   := $(OBJECTS_BASE_DIR)
PLACES_SRC    := $(PLACES_BASE_DIR)
TEXTS_SRC     := $(TEXTS_BASE_DIR)

SOCIETY_SRC := $(SOCIETY_BASE_DIR)

TCP_SERVERS_SRC := $(TCP_SERVERS_BASE_DIR)/src
UDP_SERVERS_SRC := $(UDP_SERVERS_BASE_DIR)/src

CLIENT_MANAGEMENT_SRC := $(CLIENT_MANAGEMENT_BASE_DIR)/src
DATABASE_STORAGE_SRC  := $(DATABASE_STORAGE_BASE_DIR)/src



# Include path section.

TCP_CLIENTS_INC = -I$(TCP_CLIENTS_SRC)
UDP_CLIENTS_INC = -I$(UDP_CLIENTS_SRC)

CREATURES_INC = -I$(CREATURES_SRC)
OBJECTS_INC   = -I$(OBJECTS_SRC)
PLACES_INC    = -I$(PLACES_SRC)
TEXTS_INC     = -I$(TEXTS_SRC)

SOCIETY_INC = -I$(SOCIETY_SRC)

TCP_SERVERS_INC = -I$(TCP_SERVERS_SRC)
UDP_SERVERS_INC = -I$(UDP_SERVERS_SRC)

CLIENT_MANAGEMENT_INC = -I$(CLIENT_MANAGEMENT_SRC)
DATABASE_STORAGE_INC  = -I$(DATABASE_STORAGE_SRC)


INC += $(TCP_CLIENTS_INC) $(UDP_CLIENTS_INC)                \
   $(CREATURES_INC) $(OBJECTS_INC) $(PLACES_INC) $(SOCIETY_INC) $(TEXTS_INC) \
   $(TCP_SERVERS_INC) $(UDP_SERVERS_INC)                    \
   $(CLIENT_MANAGEMENT_INC) $(DATABASE_STORAGE_INC)



# BEAM path section.

TCP_CLIENTS_BEAM := $(TCP_CLIENTS_SRC)
UDP_CLIENTS_BEAM := $(UDP_CLIENTS_SRC)

CREATURES_BEAM := $(CREATURES_SRC)
OBJECTS_BEAM   := $(OBJECTS_SRC)
PLACES_BEAM    := $(PLACES_SRC)
TEXTS_BEAM     := $(TEXTS_SRC)

SOCIETY_BEAM := $(SOCIETY_SRC)

SERVERS_BEAM := $(SERVERS_SRC)

TCP_SERVERS_BEAM := $(TCP_SERVERS_SRC)
UDP_SERVERS_BEAM := $(UDP_SERVERS_SRC)

FUNCTIONAL_BEAM  := $(FUNCTIONAL_SRC)

CLIENT_MANAGEMENT_BEAM := $(CLIENT_MANAGEMENT_SRC)
DATABASE_STORAGE_BEAM  := $(DATABASE_STORAGE_SRC)


BEAM_DIRS += \
	$(TCP_CLIENTS_BEAM) $(UDP_CLIENTS_BEAM)                                \
	$(CREATURES_BEAM) $(OBJECTS_BEAM) $(PLACES_BEAM) $(TEXTS_BEAM)        \
	$(SOCIETY_BEAM)                      \
	$(SERVERS_BEAM) $(TCP_SERVERS_BEAM) $(UDP_SERVERS_BEAM)                \
	$(FUNCTIONAL_BEAM) $(CLIENT_MANAGEMENT_BEAM) $(DATABASE_STORAGE_BEAM)  \
	$(EGEOIP_ROOT)/ebin


# See corresponding firewall rules in 'iptables.rules-Gateway.sh':
TCP_FIREWALL_LOW  := 51000
TCP_FIREWALL_HIGH := 51999
TCP_FIREWALL_OPT = --tcp-range $(TCP_FIREWALL_LOW) $(TCP_FIREWALL_HIGH)


# Orge-specific port:
EPMD_PORT = 4269
EPMD_PORT_OPT = --epmd-port $(EPMD_PORT)


# Defined intentionally to avoid command-line cookies:
# (Orge is expected to rely on file-based cookies instead)
NO_DEFAULT_COOKIE_WANTED="true"


OVERRIDDEN_OPT := $(OVERRIDDEN_OPT) $(TCP_FIREWALL_OPT) $(EPMD_PORT_OPT)


ORGE_WEB_INSTALL_DIR = $(LOANI_INSTALLATIONS_ROOT)/OSDL-$(OSDL_TARGET_VERSION)/share/OSDL/doc/web/main/documentation/OSDL/Orge


# We define the path to the Traces layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(ORGE_TOP)/_checkouts/traces/.),)
	TRACES_TOP = $(ORGE_TOP)/_checkouts/traces
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(ORGE_TOP)/../traces/.),)
		TRACES_TOP = $(ORGE_TOP)/../traces
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(ORGE_TOP)/_build/default/lib/traces/.),)
			TRACES_TOP = $(ORGE_TOP)/_build/default/lib/traces
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Traces from '$(realpath .)')
		endif
	endif
endif

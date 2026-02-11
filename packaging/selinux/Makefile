# Makefile for EXWM-VR SELinux policy module
#
# Build:   make
# Install: make install
# Clean:   make clean
# Test:    make check

POLICY_NAME := exwm_vr
POLICY_VER  := 0.1.0

# SELinux policy development paths (RHEL/Rocky)
SHAREDIR    := /usr/share/selinux
DEVEL_DIR   := $(SHAREDIR)/devel
DEVEL_MK    := $(DEVEL_DIR)/Makefile

# Tools
SEMODULE    := /usr/sbin/semodule
RESTORECON  := /usr/sbin/restorecon
CHECKMODULE := /usr/bin/checkmodule
SEMODULE_PKG := /usr/bin/semodule_package
LOAD_POLICY := /usr/sbin/load_policy

# Source files
TE_FILE     := $(POLICY_NAME).te
IF_FILE     := $(POLICY_NAME).if
FC_FILE     := $(POLICY_NAME).fc

# Build artifacts
PP_FILE     := $(POLICY_NAME).pp
MOD_FILE    := $(POLICY_NAME).mod

# Installation paths
INSTALL_PP  := $(SHAREDIR)/packages/$(PP_FILE)
INSTALL_IF  := $(DEVEL_DIR)/include/contrib/$(IF_FILE)

.PHONY: all build install uninstall reload relabel clean check help

all: build

# Build the policy package using the SELinux policy devel Makefile
build: $(PP_FILE)

$(PP_FILE): $(TE_FILE) $(IF_FILE) $(FC_FILE)
	@echo "==> Building SELinux policy module: $(POLICY_NAME) v$(POLICY_VER)"
	@if [ ! -f "$(DEVEL_MK)" ]; then \
		echo "ERROR: $(DEVEL_MK) not found. Install selinux-policy-devel." >&2; \
		exit 1; \
	fi
	$(MAKE) -f $(DEVEL_MK) $(PP_FILE)

# Install the compiled policy module
install: $(PP_FILE)
	@echo "==> Installing SELinux policy module: $(POLICY_NAME)"
	install -D -m 0644 $(PP_FILE) $(DESTDIR)$(INSTALL_PP)
	install -D -m 0644 $(IF_FILE) $(DESTDIR)$(INSTALL_IF)
	@if [ -z "$(DESTDIR)" ]; then \
		echo "==> Loading policy module into kernel..."; \
		$(SEMODULE) -i $(INSTALL_PP); \
	else \
		echo "==> DESTDIR set; skipping semodule load (for RPM build)"; \
	fi

# Remove the policy module from the running system
uninstall:
	@echo "==> Removing SELinux policy module: $(POLICY_NAME)"
	$(SEMODULE) -r $(POLICY_NAME) 2>/dev/null || true
	rm -f $(INSTALL_PP)
	rm -f $(INSTALL_IF)

# Reload without reinstalling (useful after manual edits)
reload: $(PP_FILE)
	@echo "==> Reloading SELinux policy module: $(POLICY_NAME)"
	$(SEMODULE) -r $(POLICY_NAME) 2>/dev/null || true
	$(SEMODULE) -i $(PP_FILE)

# Relabel all file contexts defined by this module
relabel:
	@echo "==> Restoring file contexts for EXWM-VR paths"
	$(RESTORECON) -R -v /usr/bin/ewwm-compositor 2>/dev/null || true
	$(RESTORECON) -R -v /usr/bin/monado-service 2>/dev/null || true
	$(RESTORECON) -R -v /usr/libexec/monado-service 2>/dev/null || true
	$(RESTORECON) -R -v /opt/exwm-vr/ 2>/dev/null || true
	$(RESTORECON) -R -v /run/exwm-vr/ 2>/dev/null || true
	$(RESTORECON) -R -v /var/lib/exwm-vr/ 2>/dev/null || true
	$(RESTORECON) -R -v /var/log/exwm-vr/ 2>/dev/null || true
	$(RESTORECON) -R -v /etc/exwm-vr/ 2>/dev/null || true

# Validate the policy source files without building
check: $(TE_FILE) $(IF_FILE) $(FC_FILE)
	@echo "==> Checking policy source syntax"
	@# Verify TE file compiles to a module
	$(CHECKMODULE) -M -m -o /dev/null $(TE_FILE) 2>&1 && \
		echo "  $(TE_FILE): OK" || \
		echo "  $(TE_FILE): FAILED"
	@# Verify FC file is parseable
	@awk 'NF && !/^#/ { \
		if ($$0 !~ /gen_context/) { \
			print "  WARN: " FILENAME ":" NR ": missing gen_context"; \
		} \
	}' $(FC_FILE)
	@echo "==> Syntax check complete"

# Remove build artifacts
clean:
	@echo "==> Cleaning build artifacts"
	rm -f $(PP_FILE) $(MOD_FILE)
	rm -f $(POLICY_NAME).pp.bz2
	rm -rf tmp/
	@# Clean artifacts from the SELinux devel Makefile
	rm -f *.mod.fc

help:
	@echo "EXWM-VR SELinux Policy Module"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the policy module (default)"
	@echo "  build     - Build $(PP_FILE) from source"
	@echo "  install   - Install and load the policy module"
	@echo "  uninstall - Remove the policy module"
	@echo "  reload    - Rebuild and reload the module"
	@echo "  relabel   - Restore file contexts on EXWM-VR paths"
	@echo "  check     - Validate source syntax"
	@echo "  clean     - Remove build artifacts"
	@echo "  help      - Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  DESTDIR   - Staging root for RPM builds (default: empty)"

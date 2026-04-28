## ---------------------------------------
## -- Standard boilerplate dependencies --
## ---------------------------------------

.PRECIOUS: Makefile
Makefile: $(srcdir)/Makefile.in $(top_srcdir)/config/var-def.mk \
	  $(top_srcdir)/config/setup-dep.mk \
	  $(abs_top_builddir)/config.status
	cd $(abs_top_builddir) && $(SHELL) ./config.status $(subdir)$@

CLEAN_SUBDIRS = $(addprefix clean-, $(SUBDIRS))
DISTCLEAN_SUBDIRS = $(addprefix distclean-, $(SUBDIRS))
.PHONY: clean $(CLEAN_SUBDIRS) $(DISTCLEAN_SUBDIRS)

clean: $(CLEAN_SUBDIRS)
	$(MAKE) clean-local

$(CLEAN_SUBDIRS): clean-%:
	$(MAKE) -C $* clean

distclean: $(DISTCLEAN_SUBDIRS)
	$(MAKE) distclean-local
	-rm Makefile

$(DISTCLEAN_SUBDIRS): distclean-%:
	$(MAKE) -C $* distclean

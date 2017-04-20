HAS_ELIXIR=0

include bu.mk

release: dist lint tag

distclean::
	$(verbose) $(RM_RF) doc

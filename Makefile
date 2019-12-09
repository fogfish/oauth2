.PHONY: deps

SRC = \
	apps/authorize \
	apps/token \
	apps/client \
	js/signin \
	js/account


deps:
	@npm --prefix cloud install
	@for I in $(SRC) ; do $(MAKE) -C $$I deps ; done

test:
	@npm --prefix cloud run lint
	@for I in $(SRC) ; do $(MAKE) -C $$I test ; done

dist:
	@for I in $(SRC) ; do $(MAKE) -C $$I dist VSN=${BUILD_RELEASE}; done


## @doc
##   static site deployment with AWS Gateway 
.PHONY: all deps test clean dist dist-up dist-rm

##
## Global config
APP  ?= signin
VSN  ?= $(shell test -z "`git status --porcelain`" && git describe --tags --long | sed -e 's/-g[0-9a-f]*//' | sed -e 's/-0//' || echo "`git describe --abbrev=0 --tags`-a")
SITE ?= ${VSN}.${CONFIG_DOMAIN}
ROOT ?= /oauth2/authorize
BUILD = build

##
##
all: test

deps: | node_modules

test: deps
	npm run lint

.env.production:
	@echo "# do nothing" | tee -a $@

node_modules:
	@npm install 

clean:
	-@rm .env.production
	-@rm -Rf build

##
##
dist: deps .env.production
	@PUBLIC_URL=${ROOT} npm run build

dist-up:
	@aws s3 sync ${BUILD} s3://${SITE}/${APP}/ --delete
	@find ${BUILD} -name "*.html" | cut -d'/' -f2- |\
		xargs -L 1 -I {} aws s3 cp ${BUILD}/{} s3://${SITE}/${APP}/{} --cache-control "max-age=20"

dist-rm:
	@aws s3 rm s3://${SITE}/${APP} --recursive

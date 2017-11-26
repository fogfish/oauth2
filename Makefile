##
## @doc
##   an example Makefile to build and ship erlang software
##
##   APP - identity of the application
##   ORG - identity of the organization 
##   URI - identity of the docker repository with last /  

APP = oauth2
ORG = fogfish
URI = 


include erlang.mk

##
## 
signin:
	mkdir -p /tmp/oauth2-signin/node_modules ;\
	cd apps/oauth2/priv/oauth2-signin ;\
	ln -s /tmp/oauth2-signin/node_modules node_modules;\
	npm install ;\
	PUBLIC_URL=/oauth2/authorize npm run build ;\
	rm node_modules ;\
	cd -

##
##
account:
	mkdir -p /tmp/oauth2-account/node_modules ;\
	cd apps/oauth2/priv/oauth2-account ;\
	ln -s /tmp/oauth2-account/node_modules node_modules;\
	npm install ;\
	PUBLIC_URL=/oauth2/account npm run build ;\
	rm node_modules ;\
	cd -


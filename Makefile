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

##
##  PUBLIC_URL=/oauth2/authorize npm run build

include erlang.mk

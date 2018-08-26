##
## @doc
##   an example Makefile to build and ship erlang software
##
##   APP - identity of the application
##   ORG - identity of the organization 
##   URI - identity of the docker repository with last /  

APP  = oauth2
ORG  = fogfish
URI  = 

ENV ?= dev

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

##
##
config-aws: rel/aws/resources.yml
	@aws cloudformation create-stack \
		--stack-name ${ENV}-${APP}-resources \
		--parameters ParameterKey=Env,ParameterValue=${ENV} \
		--template-body file://$< \
		--capabilities CAPABILITY_NAMED_IAM
	@aws cloudformation wait stack-create-complete \
		--stack-name ${ENV}-oauth2-resources

##
##
service-up: rel/aws/compose.yml
	ecs-cli compose \
		-cluster ${ECS} \
		--project-name ${APP} \
		--task-role-arn ${ENV}-${APP}-role \
   	--file $< \
   	service up

service-rm: rel/aws/compose.yml
	ecs-cli compose -c ${ECS} -p ${APP} -f $< service list \
		&& ecs-cli compose -c ${ECS} -p ${APP} -f $< service rm --timeout 10 \
		|| echo "service down ${APP}"

##
##
it:
	make clean
	make release
	make docker
	docker push ${ORG}/${APP}:latest

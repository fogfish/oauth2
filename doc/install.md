# Install OAuth2 appliance


## AWS Elastic Container Service

The application installation to AWS ECS requires
* AWS account
* [AWS command-line tools](https://aws.amazon.com/cli/)
* [AWS ECS command-line tools](http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_CLI.html)
* Valid access right to execute `aws` utility.

### Configure the deployment environment

```
export ENV=live
```

### Provision backing service

The appliance uses concept of backing service to attach all resource required for operation. It provides Infrastructure-as-a-Code templates to spawn required cloud resources and treats them as backing services. 

> A backing service is any service the app consumes over the network as part of its normal operation.

The following command create: DynamoDB table
```
aws cloudformation create-stack \
   --stack-name ${ENV}-oauth2-resources \
   --template-body file://./rel/aws/resources.yaml \
   --parameters ParameterKey=Env,ParameterValue=${ENV}
```

### Spawn OAuth2 service

Use ECS command line utilities to deploy oauth2 service 
```
ecs-cli compose --project-name oauth2 --file rel/aws/oauth2.yaml service up
```


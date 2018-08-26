# Install OAuth2 appliance


## AWS Elastic Container Service

The application installation requires
* AWS account
* [AWS command-line tools](https://aws.amazon.com/cli/)
* [AWS ECS command-line tools](http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_CLI.html)
* Valid access right to execute `aws` utility.

### Provision backing service

The appliance uses concept of backing service to attach all resource required for operation. It provides Infrastructure-as-a-Code templates to spawn required cloud resources and treats them as backing services. 

> A backing service is any service the app consumes over the network as part of its normal operation.

Use the following command to spawn all required service. The script uses supplied aws cloud formation [template](../rel/aws/resources.yml). As the result, you'll get a new stack `${Env}-oauth2-resources` in your console.

```
make config-aws ENV=live
```

### Configure OAuth2

OAuth2 deployment requires a configuration to address you needs. Use the [compose file]((../rel/aws/compose.yml)) as a template.

```bash
## Enable DynamoDB as storage back-end,
## You might need to configure AWS Region only
OAUTH2_STORAGE=ddb+https://dynamodb.eu-west-1.amazonaws.com:443/live-oauth2-pubkey?hashkey=access

## Server port and protocol, see Enable Secure Transport Layer if you need https
OAUTH2_PORT=http://*:8080 
OAUTH2_TLS_CERTIFICATE=none
OAUTH2_TLS_PRIVATE_KEY=none

## define claim issuer
## RFC 7519   
##    4.1.1.  "iss" (Issuer) Claim
OAUTH2_ISSUER=http://localhost:8080

## define claim audience
## RFC 7519
##   4.1.3.  "aud" (Audience) Claim
OAUTH2_AUDIENCE=any

## comma separated list of default claim scopes
OAUTH2_CLAIMS=uid=true

##
## configure token(s) time-to-live in seconds
OAUTH2_TTL_ACCESS_TOKEN=3600
OAUTH2_TTL_REFRESH_TOKEN=86400
OAUTH2_TTL_EXCHANGE_CODE=60
```

#### Enable GitHub integration

OAuth2 supports integration with GitHub. This feature is a classical account federation. See the developer guidelines for [details](https://developer.github.com/apps/building-oauth-apps/). Once, you've enabled app at GitHub console, you can configure this server  

```bash
GITHUB_ACCESS_KEY=YourAccessKey
GITHUB_SECRET_KEY=YourSecretKey
GITHUB_ORG=YourGitHubOrg
```

### Spawn OAuth2 service

The appliance supplies configuration to spawn service at AWS ECS. Just tell the cluster id at `ECS` variable and command will spawn the service

```
make service-up ENV=live ECS=mycluster
```

### Enables Secure Transport Layer

The usage of AWS ALB is the recommended approach to host OAuth2. However, this is becomes an overhead for extremely small projects. This authority server supports https protocol. Just change a protocol schema and provides certificates and private key. Note: only S3 as key source is supported. Easiest way to obtain trusted TLS certificate are [Let's Encrypt](https://letsencrypt.org) or [certbot-on-aws](https://github.com/fogfish/certbot-on-aws).

```
OAUTH2_PORT=https://*:8443
OAUTH2_TLS_CERTIFICATE=s3://letsencrypt/my.domain/cert.pem
OAUTH2_TLS_PRIVATE_KEY=s3://letsencrypt/my.domain/privkey.pem
```





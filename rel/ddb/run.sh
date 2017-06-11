#!/bin/bash

##
##
echo "==> spawn ddb"
java \
  -Djava.library.path=/usr/local/aws-ddb/DynamoDBLocal_lib \
  -jar /usr/local/aws-ddb/DynamoDBLocal.jar \
  -sharedDb \
  -dbPath /var/lib/aws-ddb/ &

##
##
echo "==> config ddb"
DDBTABLE=oauth2pubkey
SERVICE=http://localhost:8000
export AWS_ACCESS_KEY_ID="aws-access-key-id"
export AWS_SECRET_ACCESS_KEY="aws-secret-key"

aws dynamodb describe-table \
   --table-name ${DDBTABLE} \
   --endpoint-url ${SERVICE} \
   --region aws-region \
&& echo " " \
|| aws dynamodb create-table \
   --table-name ${DDBTABLE} \
   --attribute-definitions AttributeName=access,AttributeType=S \
   --key-schema AttributeName=access,KeyType=HASH \
   --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 \
   --endpoint-url ${SERVICE} \
   --region aws-region

wait

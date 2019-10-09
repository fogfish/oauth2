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
SERVICE=http://localhost:8000
export AWS_ACCESS_KEY_ID="aws-access-key-id"
export AWS_SECRET_ACCESS_KEY="aws-secret-key"

##
##
DDB_PUBKEY=pubkey
aws dynamodb describe-table \
   --table-name ${DDB_PUBKEY} \
   --endpoint-url ${SERVICE} \
   --region aws-region \
&& echo " " \
|| aws dynamodb create-table \
   --table-name ${DDB_PUBKEY} \
   --attribute-definitions \
      AttributeName=prefix,AttributeType=S \
      AttributeName=suffix,AttributeType=S \
   --key-schema \
      AttributeName=prefix,KeyType=HASH \
      AttributeName=suffix,KeyType=RANGE \
   --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 \
   --endpoint-url ${SERVICE} \
   --region aws-region

wait

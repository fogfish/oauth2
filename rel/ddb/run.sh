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
DDB_PUBKEY=oauth2pubkey
aws dynamodb describe-table \
   --table-name ${DDB_PUBKEY} \
   --endpoint-url ${SERVICE} \
   --region aws-region \
&& echo " " \
|| aws dynamodb create-table \
   --table-name ${DDB_PUBKEY} \
   --attribute-definitions \
      AttributeName=access,AttributeType=S \
      AttributeName=master,AttributeType=S \
   --key-schema \
      AttributeName=access,KeyType=HASH \
   --global-secondary-indexes \
      IndexName=master,KeySchema=["{AttributeName=master,KeyType=HASH}"],Projection="{ProjectionType=INCLUDE,NonKeyAttributes=[access]}",ProvisionedThroughput="{ReadCapacityUnits=5,WriteCapacityUnits=5}" \
   --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 \
   --endpoint-url ${SERVICE} \
   --region aws-region

wait


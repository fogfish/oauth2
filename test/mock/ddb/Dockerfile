FROM centos

##
## Install basic utilities, java and python
## we need python to create dynamo tables
RUN set -e \
   && yum -y -q install \
      java \
      python \
      python-setuptools \
   && easy_install -U pip \
   && pip install --no-cache-dir -U awscli

##
## Install DynamoDB mock
RUN set -e \
   && mkdir /usr/local/aws-ddb \
   && cd /usr/local/aws-ddb \
   && curl -L -O http://dynamodb-local.s3-website-us-west-2.amazonaws.com/dynamodb_local_latest.tar.gz \
   && tar -xvf dynamodb_local_latest.tar.gz \
   && rm dynamodb_local_latest.tar.gz \
   && mkdir /var/lib/aws-ddb 

EXPOSE 8000

COPY run.sh /usr/local/aws-ddb/
ENTRYPOINT ["bash", "/usr/local/aws-ddb/run.sh"]

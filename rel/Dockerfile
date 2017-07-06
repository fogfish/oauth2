FROM centos

##
## install dependencies required for bundle deployment
RUN set -e \
   && yum -y update  \
   && yum -y install \
      iproute \
      tar  \
      unzip \
      openssl

##
## 
ENV   ARCH  x86_64
ENV   PLAT  Linux
ARG   APP=oauth2
ARG   VSN=

##
## install application
COPY ${APP}-${VSN}+${ARCH}.${PLAT}.bundle /tmp/${APP}.bundle

RUN set -e \
   && sh /tmp/${APP}.bundle \
   && rm /tmp/${APP}.bundle

ENV PATH $PATH:/usr/local/oauth2/bin/

ENTRYPOINT /etc/init.d/oauth2 foreground

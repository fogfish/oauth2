# OAuth2 

The microservice based implementation of OAuth 2.0 Authorization Framework, [RFC 6749](https://tools.ietf.org/html/rfc6749). It provides an out-of-the-box, cross-platform solution for identity management. This appliance implements an automated and immutable operation of the OAuth 2.0 framework and all required cloud resources. It provides Infrastructure-as-a-Code templates to spawn required cloud resources and treats them as backing services from the appliance perspective.

[![Build Status](https://secure.travis-ci.org/fogfish/oauth2.svg?branch=master)](http://travis-ci.org/fogfish/oauth2) [![GitHub release](https://img.shields.io/github/release/fogfish/oauth2.svg)](https://github.com/fogfish/oauth2/releases/latest) [![Coverage Status](https://coveralls.io/repos/github/fogfish/oauth2/badge.svg?branch=master)](https://coveralls.io/github/fogfish/oauth2?branch=master)

## Key Features and Functionality

Work In Progress

**OAuth 2.0 grants flow**: It supports out of the box grants defined by RFC 6749: Authorization code grant, Implicit grant, Client credentials grant, Resource owner password credentials grant and Refresh Token grant.

**Required client identity**: [RFC 6749](https://tools.ietf.org/html/rfc6749#section-3.2.1) discusses about client authentication. This implementation requires HTTP basic digest schema to identity confidential clients and demands `client_id` parameter to identify public clients when sending requests to service endpoints. 


## Inspiration

The appliance architecture and design reflect the principles of incremental scalability, decentralization and fault tolerance. The appliance targets no configuration experience for cloud operation and deployment. 



## Getting Started

The appliance supplies pre-built releases for Linux/x86_64 and Docker platforms. Instructions for using these binaries are on the [GitHub releases page](https://github.com/fogfish/oauth2/releases).

Build the latest version of authorization server from the `master` branch. The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.


### Running authorization server

The easiest way to run the appliance is with the Docker container. The option is viable only if you have configured Docker development environment on your platform. Use the latest [release version](https://github.com/fogfish/oauth2/releases):

```
docker-compose -f rel/local.yaml up
```

This starts a local instances of required backing services (e.g. DynamoDB, Redis, etc), authorization service itself and exposed OAuth 2.0 services using REST API on port 8080. By default, it is bound to `localhost` on Mac OS and Linux. If you're using a different platform, please check your Docker configuration.

Open `http://localhost:8080/oauth2/authorize` in your web browser to manage accounts and integrate OAuth 2.0 clients. 


## Next Steps

* study [The OAuth 2.0 Authorization Framework](https://tools.ietf.org/html/rfc6749) and its authorization flows.
* [install guidelines](docs/install.md) to the cloud for production operation.


## Contribution

OAuth 2.0 is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later and essential build tools.

**Build** and **run** authorization service in your development console. The following command boots Erlang virtual machine and opens Erlang shell.

```
git clone https://github.com/fogfish/oauth2
cd oauth2
make
make run
```

The development of authorization server requires ensemble of **backing services** (e.g. DynamoDB, Redis, etc).

```
docker-compose -f rel/service.yaml up
```

Now you are able to start oauth2 is **debug** mode. You shall be able to use OAuth 2.0 REST API once you launch application:

```erlang
oauth2:start().
```

**Package** the application into bundle tar-ball archive, which is as-is deployable to any host. 

```
make clean
make pkg PLAT=Linux
```
The archive `oauth2-{vsn}.{arch}.{plat}.bundle` contains both a Erlang VM, all required dependencies and the application.

You can package this archive into the docker **container** for deployment purposes

```
make docker
```


### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with OAuth 2.0 appliance, please let us know via [GitHub issues](https://github.com/fogfish/oauth2/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem.


### security issues

If you discover any security related issues, please [email](mailto:dmkolesnikov@gmail.com) instead of using the issue tracker. 

## Changelog

The appliance uses [semantic versions](http://semver.org) to identify stable releases. 

* 0.0.0 - initial release for testing purpose 

## References

1. [The OAuth 2.0 Authorization Framework](https://tools.ietf.org/html/rfc6749)
1. [OAuth 2.0 Token Introspection](https://tools.ietf.org/html/rfc7662)
1. [JSON Web Token (JWT)](https://tools.ietf.org/html/rfc7519)


## License

Copyright 2017 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
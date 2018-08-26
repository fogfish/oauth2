##
## Copyright (C) 2012 Dmitry Kolesnikov
##
## This Dockerfile may be modified and distributed under the terms
## of the MIT license.  See the LICENSE file for details.
## https://github.com/fogfish/makefile
##
## @doc
##   This dockerfile is a reference container for Erlang releases
##
## @version 1.0.0
FROM fogfish/erlang-alpine-rt:20.3

COPY _build/default/rel /rel
ENTRYPOINT spawn-erlang-node oauth2

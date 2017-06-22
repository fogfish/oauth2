%%
%%   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%
-module(oauth2_kvs_client_SUITE).
-include_lib("common_test/include/ct.hrl").

%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2
]).

-export([create/1, lookup/1, remove/1, check_type/1, check_redirect_uri/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, kvs}
   ].

groups() ->
   [
      %%
      %% 
      {kvs, [parallel], 
         [create, lookup, remove, check_type, check_redirect_uri]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   ok = pts:start(),
   {ok, Pid} = pts:start_link(oauth2client, [
      'read-through',
      {factory, temporary},
      {entity,  {oauth2_kvs_client, start_link, [undefined]}}
   ]),
   erlang:unlink(Pid),
   Config.

end_per_suite(_Config) ->
   erlang:exit(whereis(oauth2client), shutdown),
   application:stop(pts),
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

%%
create(_Config) ->
   {ok, #{
      <<"access">> := <<"create">>
     ,<<"type">> := <<"public">>
     ,<<"redirect_uri">> := <<"http://localhost:8080/path">>
   }} = oauth2_kvs_client:create(<<"create">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }).

%%
lookup(_Config) ->
   {ok, _} = oauth2_kvs_client:create(<<"lookup">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }),
   {ok, #{
      <<"access">> := <<"lookup">>
     ,<<"type">> := <<"public">>
     ,<<"redirect_uri">> := <<"http://localhost:8080/path">>
   }} = oauth2_kvs_client:lookup(<<"lookup">>).

%%
remove(_Config) ->
   {ok, _} = oauth2_kvs_client:create(<<"remove">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }),
   {ok, #{
      <<"access">> := <<"remove">>
     ,<<"type">> := <<"public">>
     ,<<"redirect_uri">> := <<"http://localhost:8080/path">>
   }} = oauth2_kvs_client:remove(<<"remove">>),
   {error, not_found} = oauth2_kvs_client:lookup(<<"remove">>).
   
%%
check_type(_Config) ->
   {ok, _} = oauth2_kvs_client:create(<<"type_1">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }),
   {ok, _} = oauth2_kvs_client:create(<<"type_2">>, #{
      <<"type">> => <<"confidential">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }),
   {error, invalid_type} = oauth2_kvs_client:create(<<"type_3">>, #{
      <<"type">> => <<"some">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }).

%%
check_redirect_uri(_Config) ->
   {ok, _} = oauth2_kvs_client:create(<<"redirect_uri_1">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path">>
   }),
   {error, invalid_uri} = oauth2_kvs_client:create(<<"redirect_uri_2">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"/path">>
   }),
   {error, invalid_uri} = oauth2_kvs_client:create(<<"redirect_uri_3">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"file:///path">>
   }),
   {error, invalid_uri} = oauth2_kvs_client:create(<<"redirect_uri_4">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080">>
   }),
   {error, invalid_uri} = oauth2_kvs_client:create(<<"redirect_uri_5">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path?a=2">>
   }),
   {error, invalid_uri} = oauth2_kvs_client:create(<<"redirect_uri_6">>, #{
      <<"type">> => <<"public">>
     ,<<"redirect_uri">> => <<"http://localhost:8080/path#name">>
   }).




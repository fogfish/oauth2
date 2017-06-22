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
-module(oauth2_restapi_client_SUITE).
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

-export([post/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, restapi}
   ].

groups() ->
   [
      %%
      %% 
      {restapi, [parallel], 
         [post]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   permit:start(),
   application:set_env(permit, issuer, "test"),
   application:set_env(permit, secret, "xxxx"),
   {ok, Pid} = oauth2_sup:start_link(),
   erlang:unlink(Pid),
   Config.

end_per_suite(_Config) ->
   erlang:exit(whereis(oauth2_sup), shutdown),
   application:stop(permit),
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
post(_Config) ->
   {ok, Token} = permit:create(<<"post">>, <<"secret">>, [oauth2devel]),
   Req  = <<"type=public&redirect_uri=http%3a%2f%2fa.b.c%2fpath">>,
   Uri  = uri:new("http://localhost:8080/oauth2/authorize"),
   Head = [{'Authorization', <<"Bearer ", Token/binary>>}],
   {ok, Json} = oauth2_restapi_client:'POST'(undefined, Req, {Uri, Head, []}),
   #{
      <<"access">> := Access,
      <<"secret">> := Secret
   } = jsx:decode(Json, [return_maps]),
   {ok, _} = permit:auth(Access, Secret).

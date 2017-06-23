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
-module(oauth2_restapi_auth_SUITE).
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

-export([
   oauth2_code_grant_flow/1
]).

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
         [oauth2_code_grant_flow]}
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
   application:set_env(permit, roles,  "uid"),

   {ok, Pid} = permit:ephemeral(),
   erlang:unlink(Pid),
   Config.

end_per_suite(_Config) ->
   erlang:exit(erlang:whereis(permit), kill),
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

oauth2_code_grant_flow(_Config) ->
   %%
   %% register user and client
   {ok, Token} = permit:create(<<"access1">>, <<"secret1">>),
   {ok, #{
      <<"access">> := Access,
      <<"secret">> := Secret
   }} = permit:pubkey(Token,
      #{
         <<"oauth2client">> => <<"confidential">>,
         <<"redirect_uri">> => <<"http://localhost:8080/path">>
      }
   ),
   
   %%
   %% build request
   Digest = base64:encode(<<Access/binary, $:, Secret/binary>>),
   Header = [{'Authorization', <<"Basic ", Digest/binary>>}],
   OAuth2 = <<"response_type=code&access=access1&secret=secret1&client_id=", Access/binary, "&state=test">>, 
   
   %%
   %% test end-point and validate data
   {302, Http, _} = oauth2_restapi_auth:'POST'(any, OAuth2, {uri:new("/"), Header, []}),
   Uri = uri:new( lens:get(lens:pair('Location'), Http) ),
   {<<"localhost">>, 8080} = uri:authority(Uri),
   <<"/path">> = uri:path(Uri),
   <<"test">> = lens:get(lens:pair(<<"state">>), uri:q(Uri)),
   Code = lens:get(lens:pair(<<"code">>), uri:q(Uri)),
   {ok, #{
      <<"sub">> := <<"access1">>
   }} = permit:validate(Code).



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
%%   oauth2 key/val storage interface for client profiles
-module(oauth2_client).
-compile({parse_transform, category}).


%%
%% client management interface
-export([
   create/2,
   create/3,
   lookup/1,
   lookup/2,
   remove/1,
   remove/2,
   claims/1,
   is_public/1,
   is_confidential/1,
   is_master/2,
   redirect_uri/2
]).

%%
%% list of valid claims for client
-define(CLAIMS,      [<<"name">>, <<"type">>, <<"redirect_uri">>, <<"security">>]).
-define(NOT_ALLOWED, [<<"secret">>, <<"nonce">>]).

-define(TYPE,        [<<"oauth2:app">>]).

%%
%% create new client profile
-spec create(_, permit:claims()) -> {ok, _} | {error, _}.
-spec create(permit:access(), permit:secret(), permit:claims()) -> {ok, _} | {error, _}.

create(Access, Secret, Claims) ->
   [either ||
      claims(Claims),
      permit:create(Access, Secret, _)
   ].

create(#{<<"sub">> := Master}, Claims) ->
   [either ||
      claims(Claims#{<<"type">> => <<"oauth2:client">>}),
      permit:pubkey(Master, _)
   ].

%%
%%
-spec lookup(permit:access()) -> {ok, _} | {error, _}.

lookup(Access) ->
   [either ||
      permit:lookup(Access),
      cats:unit(maps:without(?NOT_ALLOWED, _)),
      claims_type(_),
      claims_security(_),
      claims_redirect_uri(_)
   ].

lookup(#{<<"sub">> := Master}, Access) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:is_master(_, Master),
      cats:unit(maps:without(?NOT_ALLOWED, _)),
      claims_type(_),
      claims_security(_),
      claims_redirect_uri(_)
   ].

%%
%%
-spec remove(_, permit:access()) -> {ok, _} | {error, _}.
-spec remove(permit:access()) -> {ok, _} | {error, _}.

remove(Access) ->
   permit:revoke(Access).


remove(#{<<"sub">> := Master}, Access) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:is_master(_, Master),
      oauth2_client:remove(Access),
      cats:unit(maps:without(?NOT_ALLOWED, _)),
      claims_type(_),
      claims_security(_),
      claims_redirect_uri(_)
   ].

%%
%%
-spec claims(_) -> {ok, _} | {error, _}.

claims(Claims) ->
   [either ||
      cats:unit(maps:with(?CLAIMS, Claims)),
      claims_type(_),
      claims_security(_),
      claims_redirect_uri(_)
   ].

%%
claims_type(#{<<"type">> := <<"oauth2:client">>} = Claims) ->
   {ok, Claims};
claims_type(_) ->
   {error, invalid_claims}.

%%
claims_security(#{<<"security">> := <<"public">>} = Claims) ->
   {ok, Claims};
claims_security(#{<<"security">> := <<"confidential">>} = Claims) ->
   {ok, Claims};
claims_security(_) ->
   {error, invalid_security_profile}.

%%
claims_redirect_uri(#{<<"redirect_uri">> := Uri} = Profile) ->
   [either ||
      cats:unit( uri:new(Uri) ),
      is_some(fun uri:schema/1, _),
      is_some(fun uri:authority/1, _),
      is_some(fun uri:path/1, _),
      is_none(fun uri:q/1, _),
      is_none(fun uri:anchor/1, _),
      cats:unit(Profile)
   ].

is_some(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {error, invalid_uri};
      {<<>>, undefined} ->
         {error, invalid_uri};
      _ ->
         {ok, Uri}
   end.

is_none(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {ok, Uri};
      _ ->
         {error, invalid_uri}
   end.

%%
%%
-spec is_public(permit:claims()) -> {ok, permit:claims()} | {error, _}.

is_public(#{<<"type">> := <<"oauth2:client">>, <<"security">> := <<"public">>} = Claims)  ->
   {ok, Claims};
is_public(_) ->
   {error, invalid_security_profile}.

%%
%%
-spec is_confidential(permit:claims()) -> {ok, permit:claims()} | {error, _}.

is_confidential(#{<<"type">> := <<"oauth2:client">>, <<"security">> := <<"confidential">>} = Claims)  ->
   {ok, Claims};
is_confidential(_) ->
   {error, invalid_security_profile}.

%%
%%
-spec is_master(permit:claims(), _) -> {ok, permit:claims()} | {error, _}.
         
is_master(#{<<"master">> := Master} = Claims, Master) ->
   {ok, Claims};
is_master(_, _) ->
   {error, invalid_master}.

%%
%%
-spec redirect_uri(permit:claims(), _) -> {ok, _} | {error, _}.

redirect_uri(#{<<"type">> := <<"oauth2:client">>, <<"redirect_uri">> := Uri}, Query) ->
   {ok, uri:s( uri:q(Query, uri:new(Uri)) )};
redirect_uri(_, _) ->
   {error, invalid_claims}.


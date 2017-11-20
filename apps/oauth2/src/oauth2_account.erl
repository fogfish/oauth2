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
%%   account management 
-module(oauth2_account).
-compile({parse_transform, category}).

-export([
   create/3,
   lookup/1,
   remove/1,
   claims/1,
   apps/1
]).

%%
%% black list of properties (claims) to exclude
-define(NOT_ALLOWED,   [<<"secret">>, <<"nonce">>]).

%%
%%
-spec create(permit:access(), permit:secret(), permit:claims()) -> {ok, _} | {error, _}.

create(Access, Secret, Claims) ->
   [either ||
      claims(Claims),
      permit:create(Access, Secret, _)
   ].

%%
%%
-spec lookup(permit:access()) -> {ok, _} | {error, _}.

lookup(Access) ->
   [either ||
      permit:lookup(Access),
      cats:unit(maps:without(?NOT_ALLOWED, _)),
      claims_type(_)
   ].

%%
%%
-spec remove(permit:access()) -> {ok, _} | {error, _}.

remove(Access) ->
   permit:revoke(Access).

%%
%%
-spec claims(_) -> {ok, _} | {error, _}.

claims(Claims) ->
   [either ||
      cats:unit(maps:without(?NOT_ALLOWED, Claims)),
      claims_type(_)
   ].

%%
claims_type(#{<<"type">> := <<"oauth2:account">>} = Claims) ->
   {ok, Claims};
claims_type(_) ->
   {error, invalid_claims}.


%%
%% return list of apps owner by the account
-spec apps(_) -> datum:either([_]).

apps(#{<<"sub">> := Access}) ->
   apps(Access);

apps(Access) ->
   [either ||
      pts:call(permit, Access, pubkey),
      cats:sequence(apps_lookup(_))
   ].

apps_lookup(List) ->
   lists:map(
      fun(#{<<"access">> := Access}) ->
         oauth2_client:lookup(Access)
      end,
      List
   ).

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
-module(oauth2_kvs_client).
-compile({parse_transform, category}).


%%
%% client management interface
-export([
   create/2,
   lookup/1,
   remove/1,
   validate/1
]).

%% ephemeral storage for client profile
-export([start_link/3, init/1, free/2, none/3, some/3]).

%%
%% 
-define(PROPERTY, [<<"access">>, <<"redirect_uri">>, <<"type">>]).


%%
%% create new client profile
-spec create(permit:access(), _) -> {ok, _} | {error, _}.

create(Access, Profile) ->
   [either ||
      fmap(Profile#{<<"access">> => Access}),
      validate(_),
      pts:put(oauth2client, Access, _)
   ].

%%
%%
-spec lookup(permit:access()) -> {ok, _} | {error, _}.

lookup(Access) ->
   pts:get(oauth2client, Access).   

%%
%%
-spec remove(permit:access()) -> {ok, _} | {error, _}.

remove(Access) ->
   pts:remove(oauth2client, Access).

%%
%%
-spec validate(_) -> {ok, _} | {error, _}.

validate(Profile) ->
   [either ||
      fmap(maps:with(?PROPERTY, Profile)),
      validate_client_type(_),
      validate_redirect_uri(_)
   ].

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% 2.1. Client Types
-spec validate_client_type(map()) -> {ok, map()} | {error, _}.

validate_client_type(#{<<"type">> := <<"public">>} = Profile) ->
   {ok, Profile};
validate_client_type(#{<<"type">> := <<"confidential">>} = Profile) ->
   {ok, Profile};
validate_client_type(_) ->
   {error, invalid_type}.

%%
%% 
-spec validate_redirect_uri(map()) -> {ok, map()} | {error, _}.

validate_redirect_uri(#{<<"redirect_uri">> := Uri} = Profile) ->
   [either ||
      fmap( uri:new(Uri) ),
      is_some(fun uri:schema/1, _),
      is_some(fun uri:authority/1, _),
      is_some(fun uri:path/1, _),
      is_none(fun uri:q/1, _),
      is_none(fun uri:anchor/1, _),
      fmap(Profile)
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

%%-----------------------------------------------------------------------------
%%
%% ephemeral storage for client profile(s)
%%
%%-----------------------------------------------------------------------------

start_link(_Uri, Ns, Access) ->
   pipe:start_link(?MODULE, [Ns, Access], []).

init([Ns, Access]) ->
   pns:register(Ns, Access, self()),
   {ok, none, undefined}.

free(_, _State) ->
   ok.

%%
none({put, _Access, State}, Pipe, _) ->
   pipe:ack(Pipe, {ok, State}),
   {next_state, some, State};

none({get, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State};

none({remove, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State}.

%%
some({put, _Access, _PubKey}, Pipe, State) ->
   pipe:ack(Pipe, {error, conflict}),
   {next_state, some, State};

some({get, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {ok, State}),
   {next_state, some, State};

some({remove, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {ok, State}),
   {stop, normal, State}.



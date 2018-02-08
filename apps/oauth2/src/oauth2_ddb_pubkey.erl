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
%%   data type of client application
-module(oauth2_ddb_pubkey).
-behaviour(pipe).
-compile({parse_transform, category}).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% 
-export([
   start_link/3,
   init/1, 
   free/2, 
   none/3,
   some/3
]).

%%
%%
-record(state, {
   ddb = undefined :: _  %% identity attribute for bucket  
  ,key = undefined :: _  %%
  ,val = undefined :: _  %%
}).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Uri, Ns, Key) ->
   pipe:start_link(?MODULE, [Uri, Ns, Key], []).

init([Uri, Ns, Key]) ->
   pns:register(Ns, Key, self()),
   case 
      [either ||
         oauth2_ddb:new(uri:new(Uri)),
         cats:unit(#state{ddb = _, key = Key}),
         checkout(_)
      ]
   of
      {ok, #state{val = undefined} = State} ->
         {ok, none, State};
      {ok, State} ->
         {ok, some, State}
   end.

free(_, _State) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% FSM
%%
%%-----------------------------------------------------------------------------

%%
none({put, _Key, Val}, Pipe, State0) ->
   case commit(State0#state{val = Val}) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, Val}),
         {next_state, some, State1};
      {error,   _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

none({get, _Key}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State};

none({remove, _Key}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State};

none(_, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State}.

%%
some({put, _Key, _Val}, Pipe, PubKey) ->
   %% Note: it is required to disable update of existed account
   %%       due to security concern
   pipe:ack(Pipe, {error, conflict}),
   {next_state, some, PubKey};

some({put, _Key, Val}, Pipe, State0) ->
   case commit(State0#state{val = Val}) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, Val}),
         {next_state, some, State1};
      {error,   _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

some({get, _Access}, Pipe, #state{val = Val} = State) ->
   pipe:ack(Pipe, {ok, Val}),
   {next_state, some, State};

some({remove, _Access}, Pipe, #state{val = Val} = State0) ->
   case revoke(State0) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, Val}),
         {stop, normal, State1};
      {error, _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

some({update, Access, PubKey}, Pipe, #state{} = State0) ->
   case commit(State0#state{val = PubKey}) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, PubKey}),
         {next_state, some, State1};
      {error,   _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

some(pubkey, Pipe, #state{ddb = Ddb, key = Access} = State) ->
   case 
      oauth2_ddb:match(Ddb, <<"master">>, [{<<"master">>, Access}])
   of 
      {ok, List} ->
         pipe:ack(Pipe, {ok, List});
      {error, _} = Error ->
         pipe:ack(Pipe, Error)
   end,
   {next_state, some, State}.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
checkout(#state{ddb = Ddb, key = Key} = State) ->
   [either ||
      oauth2_ddb:get(Ddb, Key),
      cats:unit(State#state{val = _})
   ].

%%
commit(#state{ddb = Ddb, val = Val} = State) ->
   [either ||
      oauth2_ddb:put(Ddb, Val),
      cats:unit(State)
   ].

%%
revoke(#state{ddb = Ddb, key = Key} = State) ->
   [either ||
      oauth2_ddb:remove(Ddb, Key),
      cats:unit(State)
   ].

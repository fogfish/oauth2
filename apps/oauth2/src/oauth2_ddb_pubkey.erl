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
         fmap(#state{ddb = _, key = Key}),
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

none({match, _Index, _Query}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State}.

%%
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

some({match, Index, Query}, Pipe, #state{ddb = Ddb} = State) ->
   case 
      oauth2_ddb:match(Ddb, Index, Query)
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
      fmap(State#state{val = _})
   ].

%%
commit(#state{ddb = Ddb, val = Val} = State) ->
   [either ||
      oauth2_ddb:put(Ddb, Val),
      fmap(State)
   ].

%%
revoke(#state{ddb = Ddb, key = Key} = State) ->
   [either ||
      oauth2_ddb:remove(Ddb, Key),
      fmap(State)
   ].

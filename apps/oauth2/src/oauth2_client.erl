-module(oauth2_client).
-behaviour(pipe).
-compile({parse_transform, category}).

%%
%% interface
-export([
   create/2,
   lookup/1,
   remove/1
]).

-export([
   start_link/3,
   init/1,
   free/2,
   none/3,
   some/3   
]).

%%
%%
-spec create(permit:token(), map()) -> {ok, map()} | {error, _}.

create(Token, Client) ->
   [either ||
      permit:pubkey(Token, [oauth2client]),
      fmap(maps:merge(_, Client)),
      create(_),
      fmap(jsx:encode(_))
   ].

create(#{<<"access">> := Access} = Client) ->
   pts:put(oauth2client, Access, Client).      

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

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

-record(state, {
   ddb = undefined :: _  %% identity attribute for bucket  
  ,key = undefined :: _  %%
  ,val = undefined :: _  %%
}).

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

none(_, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State}.

%%
some({put, _Key, _Val}, Pipe, State) ->
   pipe:ack(Pipe, {error, conflict}),
   {stop, normal, State};

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
   end.

%%
%%
checkout(#state{ddb = Ddb, key = Key} = State) ->
   [either ||
      oauth2_ddb:get(Ddb, Key),
      fmap(State#state{val = _})
   ].

%%
%%
commit(#state{ddb = Ddb, val = Val} = State) ->
   [either ||
      fmap(maps:with([<<"access">>, <<"redirect_uri">>, <<"type">>], Val)),
      oauth2_ddb:put(Ddb, _),
      fmap(State)
   ].

%%
%%
revoke(#state{ddb = Ddb, key = Key} = State) ->
   [either ||
      oauth2_ddb:remove(Ddb, Key),
      permit:revoke(Key),
      fmap(State)
   ].


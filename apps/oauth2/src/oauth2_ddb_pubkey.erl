%% @doc
%%    aws ddb storage back-end for pubkey pairs
-module(oauth2_ddb_pubkey).
-behaviour(pipe).
-compile({parse_transform, category}).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% 
-export([
   start_link/2, 
   init/1, 
   free/2, 
   none/3,
   pair/3
]).

%%
%%
-record(state, {
   config = undefined :: _  %% aws config 
  ,bucket = undefined :: _  %% ddb bucket
  ,access = undefined :: _  %%
  ,pubkey = undefined :: _  %%
}).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Ns, Access) ->
   pipe:start_link(?MODULE, [Ns, Access], []).

init([Ns, Access]) ->
   pns:register(Ns, Access, self()),
   case checkout(config(Access)) of
      {ok, #state{pubkey = undefined} = State} ->
         {ok, none, State};
      {ok, State} ->
         {ok, pair, State}
   end.

free(_, _PubKey) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% FSM
%%
%%-----------------------------------------------------------------------------

%%
none({put, _Access, PubKey}, Pipe, State0) ->
   case commit(State0#state{pubkey = PubKey}) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, PubKey}),
         {next_state, pair, State1};
      {error,   _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

none({update, _Access, _PubKey}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State};

none({get, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State};

none({remove, _Access}, Pipe, State) ->
   pipe:ack(Pipe, {error, not_found}),
   {stop, normal, State}.

%%
pair({put, _Access, _PubKey}, Pipe, State) ->
   pipe:ack(Pipe, {error, conflict}),
   {next_state, pair, State};

pair({update, _Access, PubKey}, Pipe, State0) ->
   case commit(State0#state{pubkey = PubKey}) of
      {ok, State1} ->
         pipe:ack(Pipe, {ok, PubKey}),
         {next_state, pair, State1};
      {error,   _} = Error ->
         pipe:ack(Pipe, Error),
         {stop, normal, State0}
   end;

pair({get, _Access}, Pipe, #state{pubkey = PubKey} = State) ->
   pipe:ack(Pipe, {ok, PubKey}),
   {next_state, pair, State};

pair({remove, _Access}, Pipe, #state{pubkey = PubKey} = State) ->
   pipe:ack(Pipe, {ok, PubKey}),
   {stop, normal, State}.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%% 
config(Access) ->
   Uri = uri:new(opts:val(ddb, oauth2)),
   {ok, Conf} = erlcloud_aws:auto_config(),
   #state{
      config = Conf#aws_config{
         ddb_scheme = scalar:c(uri:schema(Uri)) ++ "://",
         ddb_host   = scalar:c(uri:host(Uri)),
         ddb_port   = uri:port(Uri)
      },
      bucket = hd(uri:segments(Uri)),
      access = Access
   }.

%%
checkout(#state{config = Conf, bucket = Bucket, access = Access} = State) ->
   [either ||
      erlcloud_ddb2:get_item(Bucket, [{<<"access">>, Access}], [], Conf),
      decode(_),
      fmap(State#state{pubkey = _})
   ].

%%
commit(#state{config = Conf, bucket = Bucket, access = Access, pubkey = PubKey} = State) ->
   [either ||
      encode(Access, PubKey),
      erlcloud_ddb2:put_item(Bucket, _, [], Conf),
      fmap(State)
   ].


%%
decode([]) ->
   {ok, undefined};   

decode(Pairs) ->
   {ok, maps:from_list([decode_pair(X) || X <- Pairs])}.   

decode_pair({<<"access">>, _} = X) ->
   X;
decode_pair({<<"master">>, _} = X) ->
   X;
decode_pair({<<"nonce">>,  X}) ->
   {<<"nonce">>, base64:decode(X)};
decode_pair({<<"secret">>,  X}) ->
   {<<"secret">>, base64:decode(X)};
decode_pair({<<"roles">>,  X}) ->
   {<<"roles">>, binary:split(X, <<$ >>, [global, trim])}.

%%
encode(_Access, PubKey) ->
   {ok, [encode_pair(X) || X <- maps:to_list(PubKey)]}.   

encode_pair({<<"access">>, _} = X) ->
   X;
encode_pair({<<"master">>, _} = X) ->
   X;
encode_pair({<<"nonce">>,  X}) ->
   {<<"nonce">>, base64:encode(X)};
encode_pair({<<"secret">>,  X}) ->
   {<<"secret">>, base64:encode(X)};
encode_pair({<<"roles">>,  X}) ->
   {<<"roles">>, scalar:s(lists:join($ , X))}.


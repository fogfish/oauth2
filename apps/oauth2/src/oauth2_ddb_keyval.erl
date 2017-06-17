%%
%% @doc
%%   data type of client application
-module(oauth2_ddb_keyval).
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
   config = undefined :: _  %% aws config 
  ,bucket = undefined :: _  %% ddb bucket
  ,hashkey= undefined :: _  %% identity attribute for bucket  
  ,key    = undefined :: _  %%
  ,val    = undefined :: _  %%
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
   case checkout(config(uri:new(Uri), Key)) of
      {ok, #state{val = undefined} = State} ->
         {ok, none, State};
      {ok, State} ->
         {ok, some, State}
   end.

free(_, _PubKey) ->
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

some({remove, _Access}, Pipe, #state{val = Val} = State) ->
   pipe:ack(Pipe, {ok, Val}),
   {stop, normal, State};

some({match, Index, Query}, Pipe, #state{config = Conf, bucket = Bucket}=State) ->
   case 
      erlcloud_ddb2:q(Bucket, Query, [{index_name, scalar:s(Index)}], Conf)
   of 
      {ok, List} ->
         pipe:ack(Pipe, {ok, [erlang:element(2, decode(X)) || X <- List]});
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
config(Uri, Key) ->
   {ok, Conf} = erlcloud_aws:auto_config(),
   #state{
      config = Conf#aws_config{
         ddb_scheme = scalar:c(uri:schema(Uri)) ++ "://",
         ddb_host   = scalar:c(uri:host(Uri)),
         ddb_port   = uri:port(Uri)
      },
      bucket = hd(uri:segments(Uri)),
      hashkey= lens:get(lens:pair(<<"hashkey">>), uri:q(Uri)),
      key    = Key
   }.

%%
checkout(#state{config = Conf, bucket = Bucket, hashkey = HKey, key = Key} = State) ->
   [either ||
      erlcloud_ddb2:get_item(Bucket, [{HKey, Key}], [], Conf),
      decode(_),
      fmap(State#state{val = _})
   ].

%%
commit(#state{config = Conf, bucket = Bucket, key = Key, val = Val} = State) ->
   [either ||
      encode(Key, Val),
      erlcloud_ddb2:put_item(Bucket, _, [], Conf),
      fmap(State)
   ].


%%
decode([]) ->
   {ok, undefined};   

decode(Pairs) ->
   {ok, maps:from_list([decode_pair(X) || X <- Pairs])}.

decode_pair(X) ->
   X.

%%
encode(_Key, Val) ->
   {ok, [encode_pair(X) || X <- maps:to_list(Val)]}.

encode_pair({Key, Val})
 when is_list(Val) ->
   {Key, {l, Val}};

encode_pair({Key, Val}) 
 when is_binary(Val) ->
   {Key, {s, Val}};

encode_pair({Key, Val})
 when is_integer(Val) ->
   {Key, {n, Val}};

encode_pair({Key, Val})
 when is_float(Val) ->
   {Key, {n, Val}}.



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
%%   aws ddb helper
-module(oauth2_ddb).
-compile({parse_transform, category}).
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([
   new/1
  ,encode/1
  ,decode/1
  ,put/2
  ,get/2
  ,remove/2
  ,match/3
]).

%%
%% data types
-type pair()  :: {binary(), _}.
-type pairs() :: [pair()]. 

-record(ddb, {
   config = undefined :: _  %% aws config 
  ,bucket = undefined :: _  %% ddb bucket
  ,hashkey= undefined :: _  %% identity attribute for bucket     
}).

-spec new(uri:uri()) -> {ok, #ddb{}} | {error, _}.

new(Uri) ->
   [either ||
      erlcloud_aws:auto_config(),
      fmap(config_ddb_endpoint(Uri, _)),
      fmap(config_ddb_fd(Uri, _))
   ].

config_ddb_endpoint(Uri, Config) ->
   Config#aws_config{
      ddb_scheme = scalar:c(uri:schema(Uri)) ++ "://",
      ddb_host   = scalar:c(uri:host(Uri)),
      ddb_port   = uri:port(Uri)
   }.

config_ddb_fd(Uri, Config) ->
   #ddb{
      config = Config,
      bucket = hd(uri:segments(Uri)),
      hashkey= lens:get(lens:pair(<<"hashkey">>), uri:q(Uri))
   }.

%%
%% encode maps to list of ddb key-val pairs
-spec encode(map()) -> {ok, pairs()} | {error, _}.

encode(Val) ->
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

%%
%% decode list of ddb key-val pairs to maps
-spec decode(pairs()) -> {ok, map() | undefined} | {error, _}.

%%
decode([]) ->
   {ok, undefined};

decode(Pairs) ->
   {ok, maps:from_list([decode_pair(X) || X <- Pairs])}.

decode_pair(X) ->
   X.

%%
%%
-spec put(#ddb{}, _) -> {ok, _} | {error, _}.

put(#ddb{config = Config, bucket = Bucket, hashkey = HKey}, Value) ->
   [either ||
      oauth2_ddb:encode(Value),
      erlcloud_ddb2:put_item(Bucket, _, [], Config),
      fmap(lens:get(lens:map(HKey), Value))
   ].

%%
%%
-spec get(#ddb{}, _) -> {ok, map()} | {error, _}.

get(#ddb{config = Config, bucket = Bucket, hashkey = HKey}, Key) ->
   [either ||
      erlcloud_ddb2:get_item(Bucket, [{HKey, Key}], [], Config),
      oauth2_ddb:decode(_)
   ].

%%
%%
-spec remove(#ddb{}, _) -> {ok, _} | {error, _}.

remove(#ddb{config = Config, bucket = Bucket, hashkey = HKey}, Key) ->
   [either ||
      erlcloud_ddb2:delete_item(Bucket, [{HKey, Key}], [], Config),
      fmap(Key)
   ].

%%
%%
-spec match(#ddb{}, _, _) -> {ok, _} | {error, _}.

match(#ddb{config = Config, bucket = Bucket}, Index, Query) ->
   [either ||
      erlcloud_ddb2:q(Bucket, Query, [{index_name, scalar:s(Index)}], Config),
      fmap(lists:map(fun(X) -> erlang:element(2, oauth2_ddb:decode(X)) end, _))
   ].

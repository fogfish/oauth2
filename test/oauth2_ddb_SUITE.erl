%% @doc
%%
-module(oauth2_ddb_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2
]).

-export([encode/1, decode/1, new/1, put/1, get/1, remove/1, match/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, ddb}
   ].

groups() ->
   [
      %%
      %% 
      {ddb, [], 
         [encode, decode, new, put, get, remove, match]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.


end_per_suite(_Config) ->
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

%%
encode(_Config) ->
   {ok, [
      {<<"a">>, {s, <<"string">>}}
     ,{<<"b">>, {n, 10}}
     ,{<<"c">>, {n, 10.1}}
     ,{<<"d">>, {l, [<<"string">>, 10, 10.1]}}  
   ]} = oauth2_ddb:encode(#{
      <<"a">> => <<"string">>
     ,<<"b">> => 10
     ,<<"c">> => 10.1
     ,<<"d">> => [<<"string">>, 10, 10.1]
   }).

%%
decode(_Config) ->
   {ok, undefined} = oauth2_ddb:decode([]),
   {ok, #{
      <<"a">> := <<"string">>
     ,<<"b">> := 10
     ,<<"c">> := 10.1
     ,<<"d">> := [<<"string">>, 10, 10.1]
   }} = oauth2_ddb:decode([
      {<<"a">>, <<"string">>}
     ,{<<"b">>, 10}
     ,{<<"c">>, 10.1}
     ,{<<"d">>, [<<"string">>, 10, 10.1]}
   ]).

%%
new(_Config) ->
   meck:new(erlcloud_aws, [passthrough]),
   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, #aws_config{}} end),

   {ok, Ddb} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),   

   meck:unload(erlcloud_aws),

   {ddb, _, <<"oauth2pubkey">>, <<"access">>} = Ddb.
   % "http://" = Config#aws_config.ddb_scheme,
   % "localhost" = Config#aws_config.ddb_host,
   % 8000 = Config#aws_config.ddb_port.

%%
put(_Config) ->
   meck:new(erlcloud_aws, [passthrough]),
   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, #aws_config{}} end),
   meck:new(erlcloud_ddb2, [passthrough]),
   meck:expect(erlcloud_ddb2, put_item, fun(_, _, _, _) -> ok end),

   {ok, Ddb} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),
   {ok, Key} = oauth2_ddb:put(Ddb, #{<<"access">> => <<"access">>, <<"secret">> => <<"secret">>}),

   meck:unload(erlcloud_aws),
   meck:unload(erlcloud_ddb2),

   <<"access">> = Key.

%%
get(_Config) ->
   meck:new(erlcloud_aws, [passthrough]),
   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, #aws_config{}} end),
   meck:new(erlcloud_ddb2, [passthrough]),
   meck:expect(erlcloud_ddb2, get_item, 
      fun(_, _, _, _) -> 
         {ok, [{<<"access">>, <<"access">>}, {<<"secret">>, <<"secret">>}]} 
      end
   ),

   {ok, Ddb} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),
   {ok, Val} = oauth2_ddb:get(Ddb, <<"access">>),

   meck:unload(erlcloud_aws),
   meck:unload(erlcloud_ddb2),

   #{
      <<"access">> := <<"access">>
     ,<<"secret">> := <<"secret">>
   } = Val.

%%
remove(_Config) ->
   meck:new(erlcloud_aws, [passthrough]),
   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, #aws_config{}} end),
   meck:new(erlcloud_ddb2, [passthrough]),
   meck:expect(erlcloud_ddb2, delete_item, fun(_, _, _, _) -> ok end),

   {ok, Ddb} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),
   {ok, Key} = oauth2_ddb:remove(Ddb, <<"access">>),

   meck:unload(erlcloud_aws),
   meck:unload(erlcloud_ddb2),

   <<"access">> = Key.
   
%%
match(_Config) ->
   meck:new(erlcloud_aws, [passthrough]),
   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, #aws_config{}} end),
   meck:new(erlcloud_ddb2, [passthrough]),
   meck:expect(erlcloud_ddb2, q, 
      fun(_, _, _, _) -> 
         {ok, [
            [{<<"access">>, <<"access1">>}, {<<"secret">>, <<"secret1">>}]
           ,[{<<"access">>, <<"access2">>}, {<<"secret">>, <<"secret2">>}]
         ]} 
      end
   ),

   {ok, Ddb} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),
   {ok, Val} = oauth2_ddb:match(Ddb, <<"master">>, [{<<"master">>, <<"access">>}]),

   meck:unload(erlcloud_aws),
   meck:unload(erlcloud_ddb2),

   [
      #{
         <<"access">> := <<"access1">>
        ,<<"secret">> := <<"secret1">>
      },
      #{
         <<"access">> := <<"access2">>
        ,<<"secret">> := <<"secret2">>
      }
   ] = Val.


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

-export([encode/1, decode/1, new/1]).

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
      {ddb, [parallel], 
         [encode, decode, new]}
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

   {ok, Config} = oauth2_ddb:new(uri:new("http://localhost:8000/oauth2pubkey?hashkey=access")),   

   meck:unload(erlcloud_aws),

   {ddb, AwsCfg, <<"oauth2pubkey">>, <<"access">>} = Config,
   "http://" = AwsCfg#aws_config.ddb_scheme,
   "localhost" = AwsCfg#aws_config.ddb_host,
   8000 = AwsCfg#aws_config.ddb_port.


   
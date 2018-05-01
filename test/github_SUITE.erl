%% @doc
%%   unit testing of github integration
-module(github_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2,
   init_per_testcase/2,
   end_per_testcase/2
]).

-export([
   auth_url/1,
   authorize/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, github}
   ].

groups() ->
   [
      %%
      %% 
      {github, [], 
         [
            auth_url,
            authorize
         ]
      }
   ].


%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

%%
%%
init_per_suite(Config) ->
   oauth2:start(),
   {ok, _} = define_client_public(),
   Config.

end_per_suite(_Config) ->
   application:stop(oauth2),
   application:stop(permit),
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%
init_per_testcase(_, Config) ->
   meck:new(knet, [unstick, passthrough]),
   meck:new(lens, [unstick, passthrough]),
   Config.

end_per_testcase(_, _Config) ->
   meck:unload(knet),
   meck:unload(lens),
   ok. 

%%
define_client_public() ->
   define_client(<<"client.p">>, <<"nosecret">>,
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"http://example.com/path">>,
         <<"master">> => <<"account.a">>
      }
   ).

%%
%% a low level implementation to inject a client with given access/secret
define_client(Access, Secret, Claims) ->
   [either ||
      permit_pubkey:new(Access, Secret, Claims),
      cats:unit(lens:put(permit_pubkey:master(), scalar:s(<<"account.a">>), _)),
      permit_pubkey_io:create(_)
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit
%%%
%%%----------------------------------------------------------------------------   

%%
%%
auth_url(_) ->
   <<"https://github.com/login/oauth/authorize?client_id=YourAccessKey&scope=user%3Aemail%20read%3Aorg%20repo">> = github:auth_url().


%%
%%
authorize(_) ->
   meck:expect(knet, socket, fun(Url, _) -> {ok, uri:path(Url)} end), 
   meck:expect(knet, send, fun(_, _) -> ok end),
   meck:expect(knet, stream, fun(X) -> mock_github_api(X, undefined) end),   
   meck:expect(knet, stream, fun mock_github_api/2),
   meck:expect(knet, close, fun(_) -> ok end),
   meck:expect(lens, takewith, fun(_, _) -> fun(Fun, S) -> lens:fmap(fun(X) -> X end, Fun(S)) end end),

   {ok, Url} = github:account(uri:new(<<"http://localhost:8080/oauth2/external/github?state=client.p&code=xxx">>)),
   Code = uri:q(<<"code">>, undefined, uri:new(Url)),
   {ok, #{
      <<"exch">> := true,
      <<"sub">>  := <<"user">>
   }} = permit:validate(Code).


%%
%%
mock_github_api(<<"/login/oauth/access_token">>, _) ->
   stream:build([
      {200, <<"OK">>, [{<<"Content-Type">>, <<"application/json">>}]},
      <<"{\"access_token\":\"token\"}">>
   ]);

mock_github_api(<<"/user">>, _) ->
   stream:build([
      {200, <<"OK">>, [{<<"Content-Type">>, <<"application/json">>}]},
      <<"{\"login\":\"user\"}">>
   ]).



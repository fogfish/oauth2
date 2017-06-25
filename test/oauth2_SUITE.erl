%%
%%
-module(oauth2_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, monad}).

%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2
]).

-export([
   signup_code_flow_with_public_client/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, restapi}
   ].

groups() ->
   [
      %%
      %% 
      {restapi, [parallel], 
         [signup_code_flow_with_public_client]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   oauth2:start(),
   Config.

end_per_suite(_Config) ->
   application:stop(oauth2),
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
-define(PUBLIC, #{
   <<"type">> => <<"oauth2:client">>,
   <<"security">> => <<"public">>,
   <<"redirect_uri">> => <<"http://example.com/path">>
}).

%%
signup_code_flow_with_public_client(_Config) ->
   {ok, _} = oauth2_client:create("signup.cf.pcA", "nosecret", ?PUBLIC),
   Payload = request(
      do([m_http ||
         _ /= new("http://localhost:8080/oauth2/authorize"),
         _ /= x('POST'),
         _ /= h("Connection: close"),
         _ /= d("access=signup.cf.pc.uA&secret=nosecret&response_type=code&client_id=signup.cf.pcA&state=opaque&oauth2=signup"),
         _ /= r(),
         return(_)
      ])
   ),
   302  = lens:get(code(), Payload),
   http = lens:get(redirect_schema(), Payload),
   <<"example.com">> = lens:get(redirect_host(), Payload),
   <<"/path">> = lens:get(redirect_path(), Payload),
   <<"opaque">> = lens:get(oauth2_response(<<"state">>), Payload),
   Code = lens:get(oauth2_response(<<"code">>), Payload),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"signup.cf.pc.uA">>,
      <<"type">> := <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Code).


%%%----------------------------------------------------------------------------   
%%%
%%% helper
%%%
%%%----------------------------------------------------------------------------   

%%
%%
request(Request) ->
   Request(#{}).   

%%
%% http response
%%   [ [{302, _, Head, _}] | _] 
code() ->
   lens:c([lens:hd(), lens:hd(), lens:t1()]).   

redirect_schema() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:schema/1)]).

redirect_host() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:host/1)]).

redirect_path() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:path/1)]).

oauth2_response(Key) ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:q/1), lens:pair(Key)]).


lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.



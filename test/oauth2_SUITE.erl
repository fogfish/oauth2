%%
%%
-module(oauth2_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).
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
   signup_code_flow_with_public_client/1,
   signin_code_flow_with_public_client/1,

   signup_implicit_flow_with_public_client/1,
   signin_implicit_flow_with_public_client/1,

   signup_code_flow_with_confidential_client/1,
   signin_code_flow_with_confidential_client/1,

   signup_implicit_flow_with_confidential_client/1,
   signin_implicit_flow_with_confidential_client/1,

   signin_password_grant_with_public_client/1,
   signin_password_grant_with_confidential_client/1,

   signin_client_grant_with_confidential_client/1
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
         [
            signup_code_flow_with_public_client,
            signin_code_flow_with_public_client,

            signup_implicit_flow_with_public_client,
            signin_implicit_flow_with_public_client,

            signup_code_flow_with_confidential_client,
            signin_code_flow_with_confidential_client,

            signup_implicit_flow_with_confidential_client,
            signin_implicit_flow_with_confidential_client,

            signin_password_grant_with_public_client,
            signin_password_grant_with_confidential_client,

            signin_client_grant_with_confidential_client
         ]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   oauth2:start(),
   {ok, _} = oauth2_client:create("client.p", "nosecret", 
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"http://example.com/path">>
      }
   ),
   {ok, _} = oauth2_client:create("client.c", "nosecret", 
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"confidential">>,
         <<"redirect_uri">> => <<"http://example.com/path">>
      }
   ),
   {ok, _} = oauth2_account:create("account.a", "nosecret",
      #{
         <<"type">> => <<"oauth2:account">>,
         <<"uid">> => true
      }
   ),
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
-define(AUTHORIZE, "http://localhost:8080/oauth2/authorize").
-define(TOKEN,     "http://localhost:8080/oauth2/token").

%%
signup_code_flow_with_public_client(_Config) ->
   Payload = oauth2_request_public(?AUTHORIZE, #{
      access         => <<"signup.cf.pc.uA">>,
      secret         => <<"nosecret">>,
      response_type  => <<"code">>,
      client_id      => <<"client.p">>,
      state          => <<"opaque">>,
      oauth2         => <<"signup">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Code  = lens:get(lens:pair(<<"code">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"signup.cf.pc.uA">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Code).

%%
signin_code_flow_with_public_client(_Config) ->
   Payload = oauth2_request_public(?AUTHORIZE, #{
      access         => <<"account.a">>,
      secret         => <<"nosecret">>,
      response_type  => <<"code">>,
      client_id      => <<"client.p">>,
      state          => <<"opaque">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Code  = lens:get(lens:pair(<<"code">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Code).

%%
signup_implicit_flow_with_public_client(_Config) ->
   Payload = oauth2_request_public(?AUTHORIZE, #{
      access         => <<"signup.if.pc.uA">>,
      secret         => <<"nosecret">>,
      response_type  => <<"token">>,
      client_id      => <<"client.p">>,
      state          => <<"opaque">>,
      oauth2         => <<"signup">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Token = lens:get(lens:pair(<<"access_token">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"signup.if.pc.uA">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).

%%
signin_implicit_flow_with_public_client(_Config) ->
   Payload = oauth2_request_public(?AUTHORIZE, #{
      access         => <<"account.a">>,
      secret         => <<"nosecret">>,
      response_type  => <<"token">>,
      client_id      => <<"client.p">>,
      state          => <<"opaque">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Token = lens:get(lens:pair(<<"access_token">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).


%%
signup_code_flow_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?AUTHORIZE, #{
      access         => <<"signup.cf.cc.uA">>,
      secret         => <<"nosecret">>,
      response_type  => <<"code">>,
      client_id      => <<"client.c">>,
      state          => <<"opaque">>,
      oauth2         => <<"signup">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Code = lens:get(lens:pair(<<"code">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"signup.cf.cc.uA">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Code).

%%
signin_code_flow_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?AUTHORIZE, #{
      access         => <<"account.a">>,
      secret         => <<"nosecret">>,
      response_type  => <<"code">>,
      client_id      => <<"client.c">>,
      state          => <<"opaque">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Code = lens:get(lens:pair(<<"code">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Code).


%%
signup_implicit_flow_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?AUTHORIZE, #{
      access         => <<"signup.if.cc.uA">>,
      secret         => <<"nosecret">>,
      response_type  => <<"token">>,
      client_id      => <<"client.c">>,
      state          => <<"opaque">>,
      oauth2         => <<"signup">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Token = lens:get(lens:pair(<<"access_token">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"signup.if.cc.uA">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).

%%
signin_implicit_flow_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?AUTHORIZE, #{
      access         => <<"account.a">>,
      secret         => <<"nosecret">>,
      response_type  => <<"token">>,
      client_id      => <<"client.c">>,
      state          => <<"opaque">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Token = lens:get(lens:pair(<<"access_token">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">>:= <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).


%%
signin_password_grant_with_public_client(_Config) ->
   Payload = oauth2_request_public(?TOKEN, #{
      grant_type     => <<"password">>,
      username       => <<"account.a">>,
      password       => <<"nosecret">>,
      client_id      => <<"client.p">>
   }),
   #{
      <<"access_token">> := Token,
      <<"token_type">> := <<"bearer">>
   } = oauth2_token(Payload),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">> := <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).

%%
signin_password_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?TOKEN, #{
      grant_type     => <<"password">>,
      username       => <<"account.a">>,
      password       => <<"nosecret">>,
      client_id      => <<"client.c">>
   }),
   #{
      <<"access_token">> := Token,
      <<"token_type">> := <<"bearer">>
   } = oauth2_token(Payload),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"type">> := <<"oauth2:account">>,
      <<"uid">> := true
   }} = permit:validate(Token).


%%
signin_client_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?TOKEN, #{
      grant_type     => <<"client_credentials">>
   }),
   #{
      <<"access_token">> := Token,
      <<"token_type">> := <<"bearer">>
   } = oauth2_token(Payload),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"client.c">>,
      <<"type">> := <<"oauth2:client">>,
      <<"security">> := <<"confidential">>
   }} = permit:validate(Token).


%%%----------------------------------------------------------------------------   
%%%
%%% helper
%%%
%%%----------------------------------------------------------------------------   

%%
%% request oauth2 authorize end-point
oauth2_request_public(Url, Request) ->
   request(
      do([m_http ||
         _ /= new(Url),
         _ /= x('POST'),
         _ /= h("Accept: */*"),
         _ /= h("Content-Type: application/x-www-form-urlencoded"),
         _ /= h("Connection: close"),
         _ /= d(x_www_form_urlencoded(Request)),
         _ /= r(),
         return(_)
      ])
   ).

oauth2_request_confidential(Url, Request) ->
   request(
      do([m_http ||
         _ /= new(Url),
         _ /= x('POST'),
         _ /= h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         _ /= h("Accept: */*"),
         _ /= h("Content-Type: application/x-www-form-urlencoded"),
         _ /= h("Connection: close"),
         _ /= d(x_www_form_urlencoded(Request)),
         _ /= r(),
         return(_)
      ])
   ).

x_www_form_urlencoded(Request) ->
   [$. ||
      maps:to_list(Request),
      lists:map(fun({Key, Val}) -> <<(scalar:s(Key))/binary, $=, (scalar:s(Val))/binary>> end, _),
      lists:join($&, _),
      erlang:iolist_to_binary(_)
   ].

%%
%%
oauth2_grant(Payload) ->
   302  = lens:get(code(), Payload),
   http = lens:get(redirect_schema(), Payload),
   <<"example.com">> = lens:get(redirect_host(), Payload),
   <<"/path">> = lens:get(redirect_path(), Payload),
   lens:get(oauth2_response(), Payload).

%%
%%
oauth2_token(Payload) ->
   200  = lens:get(code(), Payload),
   lens:get(json(), Payload).



%%
%%
request(Request) ->
   Request(#{}).   

%%
%% http response
%%   [ [{302, _, Head, _} | Payload] | _] 
code() ->
   lens:c([lens:hd(), lens:hd(), lens:t1()]).

json() ->
   lens:c([lens:hd(), lens:tl(), lens_jsx()]).   

redirect_schema() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:schema/1)]).

redirect_host() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:host/1)]).

redirect_path() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:path/1)]).

oauth2_response() ->
   lens:c([lens:hd(), lens:hd(), lens:t3(), lens:pair('Location'), lens_uri(fun uri:q/1)]).

lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.

lens_jsx() ->
   fun(Fun, Json) ->
      lens:fmap(fun(_) -> Json end, Fun( jsx:decode(scalar:s(Json), [return_maps]) ))
   end.





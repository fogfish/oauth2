%%
%%
-module(oauth2_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

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
   signin_code_grant_with_public_client/1,
   signup_code_grant_with_public_client/1,

   signin_implicit_grant_with_public_client/1,
   signup_implicit_grant_with_public_client/1,

   signin_code_grant_with_confidential_client/1,
   signup_code_grant_with_confidential_client/1,

   signin_implicit_grant_with_confidential_client/1,
   signup_implicit_grant_with_confidential_client/1,

   signin_password_grant_with_public_client/1,
   signin_password_grant_with_confidential_client/1,

   signin_client_grant_with_confidential_client/1,

   restapi_jwks/1,
   restapi_introspect/1
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
            signin_code_grant_with_public_client,
            signup_code_grant_with_public_client,

            signin_implicit_grant_with_public_client,
            signup_implicit_grant_with_public_client,

            signin_code_grant_with_confidential_client,
            signup_code_grant_with_confidential_client,

            signin_implicit_grant_with_confidential_client,
            signup_implicit_grant_with_confidential_client,

            signin_password_grant_with_public_client,
            signin_password_grant_with_confidential_client,

            signin_client_grant_with_confidential_client,

            restapi_jwks,
            restapi_introspect
         ]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   oauth2:start(),
   {ok, _} = define_client_public(),
   {ok, _} = define_client_confidential(),
   {ok, _} = define_account(),
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
define_client_public() ->
   oauth2_client:create("client.p", "nosecret",
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"http://example.com/path">>
      }
   ).

%%
define_client_confidential() ->
   oauth2_client:create("client.c", "nosecret", 
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"confidential">>,
         <<"redirect_uri">> => <<"http://example.com/path">>
      }
   ).   

%%
define_account() ->
   oauth2_account:create("account.a", "nosecret",
      #{
         <<"type">> => <<"oauth2:account">>,
         <<"uid">> => true
      }
   ).   

%%%----------------------------------------------------------------------------
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------
-define(ENDPOINT_SIGNIN,     "http://localhost:8080/oauth2/signin").
-define(ENDPOINT_SIGNUP,     "http://localhost:8080/oauth2/signup").
-define(ENDPOINT_TOKEN,      "http://localhost:8080/oauth2/token").
-define(ENDPOINT_JWKS,       "http://localhost:8080/oauth2/jwks").
-define(ENDPOINT_INTROSPECT, "http://localhost:8080/oauth2/introspect").


%%
signin_code_grant_with_public_client(_) ->
   Payload = oauth2_request_public(?ENDPOINT_SIGNIN, #{
      access         => <<"account.a">>,
      secret         => <<"nosecret">>,
      response_type  => <<"code">>,
      client_id      => <<"client.p">>,
      state          => <<"opaque">>
   }),
   Grant = oauth2_grant(Payload),
   <<"opaque">> = lens:get(lens:pair(<<"state">>), Grant),
   Code = lens:get(lens:pair(<<"code">>), Grant),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"aud">> := <<"any">>,
      <<"exch">>:= true,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Code).

%%
signup_code_grant_with_public_client(_) ->
   Payload = oauth2_request_public(?ENDPOINT_SIGNUP, #{
      access         => <<"signup.cg.pc.uA">>,
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
      <<"sub">> := <<"signup.cg.pc.uA">>,
      <<"exch">>:= true,
      <<"aud">> := <<"any">>,
      <<"exch">>:= true,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Code).


%%
signin_implicit_grant_with_public_client(_Config) ->
   Payload = oauth2_request_public(?ENDPOINT_SIGNIN, #{
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
      <<"uid">> := true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Token).

%%
signup_implicit_grant_with_public_client(_Config) ->
   Payload = oauth2_request_public(?ENDPOINT_SIGNUP, #{
      access         => <<"signup.ig.pc.uA">>,
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
      <<"sub">> := <<"signup.ig.pc.uA">>,
      <<"uid">> := true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Token).


%%
signin_code_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_SIGNIN, #{
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
      <<"exch">>:= true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Code).


%%
signup_code_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_SIGNUP, #{
      access         => <<"signup.cg.cc.uA">>,
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
      <<"sub">> := <<"signup.cg.cc.uA">>,
      <<"exch">>:= true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Code).


%%
signin_implicit_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_SIGNIN, #{
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
      <<"uid">> := true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Token).


%%
signup_implicit_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_SIGNUP, #{
      access         => <<"signup.ig.cc.uA">>,
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
      <<"sub">> := <<"signup.ig.cc.uA">>,
      <<"uid">> := true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   }} = permit:validate(Token).



%%
signin_password_grant_with_public_client(_Config) ->
   Payload = oauth2_request_public(?ENDPOINT_TOKEN, #{
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
      <<"uid">> := true
   }} = permit:validate(Token).

%%
signin_password_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_TOKEN, #{
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
      <<"uid">> := true
   }} = permit:validate(Token).


%%
signin_client_grant_with_confidential_client(_Config) ->
   Payload = oauth2_request_confidential(?ENDPOINT_TOKEN, #{
      grant_type     => <<"client_credentials">>
   }),
   #{
      <<"access_token">> := Token,
      <<"token_type">> := <<"bearer">>
   } = oauth2_token(Payload),
   {ok, #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"client.c">>
   }} = permit:validate(Token).

%%
restapi_jwks(_) ->
   Jwks = oauth2_public_get(?ENDPOINT_JWKS),
   200  = lens:get(code(), Jwks),
   #{<<"keys">> := Keys} = lens:get(json(), Jwks),
   #{
      <<"kid">> := <<"jwt">>,
      <<"alg">> := <<"RS256">>,
      <<"kty">> := <<"RSA">>,
      <<"n">>   := _,
      <<"e">>   := _
   } = hd(Keys).

%%
restapi_introspect(_) ->
   {ok, Token} = oauth2_token:access(<<"account.a">>, <<"nosecret">>),
   Payload = oauth2_request_confidential(?ENDPOINT_INTROSPECT, #{token => Token}),
   200 = lens:get(code(), Payload),
   #{
      <<"iss">> := <<"http://localhost:8080">>,
      <<"sub">> := <<"account.a">>,
      <<"uid">> := true,
      <<"aud">> := <<"any">>,
      <<"exp">> := _,
      <<"tji">> := _
   } = lens:get(json(), Payload).



%%%----------------------------------------------------------------------------   
%%%
%%% helper
%%%
%%%----------------------------------------------------------------------------   

%%
%%
oauth2_public_get(Url) ->
   request(
      [m_http ||
         cats:new(Url),
         cats:x('GET'),
         cats:h("Accept: */*"),         
         cats:h("Connection: close"),
         cats:r()
      ]
   ).


%%
%% request oauth2 authorize end-point
oauth2_request_public(Url, Request) ->
   request(
      [m_http ||
         cats:new(Url),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),
         cats:d(x_www_form_urlencoded(Request)),
         cats:r()
      ]
   ).

oauth2_request_confidential(Url, Request) ->
   request(
      [m_http ||
         cats:new(Url),
         cats:x('POST'),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),
         cats:d(x_www_form_urlencoded(Request)),
         cats:r()
      ]
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
   lens:c(lens:hd(), lens:hd(), lens:t1()).

json() ->
   lens:c(lens:hd(), lens:tl(), lens_jsx()).   

redirect_schema() ->
   lens:c(lens:hd(), lens:hd(), lens:t3(), lens:pair(<<"Location">>), lens_uri(fun uri:schema/1)).

redirect_host() ->
   lens:c(lens:hd(), lens:hd(), lens:t3(), lens:pair(<<"Location">>), lens_uri(fun uri:host/1)).

redirect_path() ->
   lens:c(lens:hd(), lens:hd(), lens:t3(), lens:pair(<<"Location">>), lens_uri(fun uri:path/1)).

oauth2_response() ->
   lens:c(lens:hd(), lens:hd(), lens:t3(), lens:pair(<<"Location">>), lens_uri(fun uri:q/1)).

lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.

lens_jsx() ->
   fun(Fun, Json) ->
      lens:fmap(fun(_) -> Json end, Fun( jsx:decode(scalar:s(Json), [return_maps]) ))
   end.





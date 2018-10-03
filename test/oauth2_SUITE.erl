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
   end_per_suite/1
]).

-export([
   authorize/1,
   authorization_code_grant_with_public_app_signin/1,
   authorization_code_grant_with_public_app_signup/1,
   implicit_grant_with_public_app_signin/1,
   implicit_grant_with_public_app_signup/1,
   authorization_code_grant_with_invalid_public_app_signin/1,
   authorization_code_grant_with_invalid_public_app_signup/1,
   authorization_code_grant_public_access_token_invalid_code/1,
   authorization_code_grant_invalid_public_access_token/1,

   authorization_code_grant_with_confidential_app_signin/1,
   authorization_code_grant_with_confidential_app_signup/1,
   implicit_grant_with_confidential_app_signin/1,
   implicit_grant_with_confidential_app_signup/1,
   authorization_code_grant_with_invalid_confidential_app_signin/1,
   authorization_code_grant_with_invalid_confidential_app_signup/1,

   resource_owner_password_grant_with_public_app/1,
   resource_owner_password_grant_with_confidential_app/1,

   client_credentials_grant/1,

   jwks/1,
   introspect/1,
   introspect_invalid_token/1,
   introspect_invalid_client/1
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
         [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
            Test =/= module_info,
            Test =/= init_per_suite,
            Test =/= end_per_suite,
            NAry =:= 1
         ]
      }
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

init_per_suite(Config) ->
   oauth2:start(),
   {ok, _} = define_account(oauth2_fixtures:account()),
   {ok, _} = define_client(oauth2_fixtures:client_public()),
   {ok, _} = define_client(oauth2_fixtures:client_confidential()),
   Config.

end_per_suite(_Config) ->
   application:stop(oauth2),
   application:stop(permit),
   ok.


%%
%% a low level implementation to inject a client with given access/secret
define_client({Access, Secret, Claims}) ->
   [either ||
      permit_pubkey:new(Access, Secret, Claims),
      cats:unit(lens:put(permit_pubkey:master(), scalar:s(<<"account.a">>), _)),
      permit_pubkey_io:create(_)
   ].

%%
define_account({Access, Secret, Claims}) ->
   oauth2_account:create(Access, Secret, Claims).   



%%%----------------------------------------------------------------------------
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------

%%
%%
authorize(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://localhost:8080/oauth2/authorize",
         _ > "Accept: */*",
         _ > "Connection: close",

         _ < 200
      ]
   ).

%%
%%
authorization_code_grant_with_public_app_signin(_) ->
   {ok, _} = [either ||
      authorization_code_grant_public_request("http://localhost:8080/oauth2/signin", <<"account.a">>),
      authorization_code_grant_public_access_token(_, <<"account.a">>)
   ].

authorization_code_grant_with_public_app_signup(_) ->
   {ok, _} = [either ||
      authorization_code_grant_public_request("http://localhost:8080/oauth2/signup", <<"signup.cg.pc.uA">>),
      authorization_code_grant_public_access_token(_, <<"signup.cg.pc.uA">>)
   ].

authorization_code_grant_public_request(Endpoint, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST " ++ Endpoint,
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, Access, <<"client.p">>),

         _ < 302,
       Url < "Location: _",
         _ < oauth2_redirect_url(Url),
         _ < oauth2_redirect_url_with_state(Url),
         _ < oauth2_redirect_url_with_code(Url, oauth2_fixtures:code(Access)),
         _ < oauth2_redirect_url_with_code(Url)
      ]
   ).

authorization_code_grant_public_access_token(Code, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > exchange_request(Code, <<"client.p">>),

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token(Access)),
         _ < oauth2_not_defined_refresh_token()
      ]
   ).

%%
authorization_code_grant_with_invalid_public_app_signin(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/signin",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, <<"account.a">>, <<"client.invalid">>),

         _ < 401
      ]
   ).

authorization_code_grant_with_invalid_public_app_signup(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/signup",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, <<"account.a">>, <<"client.invalid">>),

         _ < 401
      ]
   ).

authorization_code_grant_public_access_token_invalid_code(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > exchange_request(<<"invalid_exchange_code">>, <<"client.p">>),

         _ < 403
      ]
   ).

authorization_code_grant_invalid_public_access_token(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > exchange_request(<<"">>, <<"client.invalid">>),

         _ < 401
      ]
   ).


%%
%%
implicit_grant_with_public_app_signin(_) ->
   {ok, _} = implicit_grant_public_request("http://localhost:8080/oauth2/signin", <<"account.a">>).

implicit_grant_with_public_app_signup(_) ->
   {ok, _} = implicit_grant_public_request("http://localhost:8080/oauth2/signup", <<"signup.ig.pc.uA">>).

implicit_grant_public_request(Endpoint, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST " ++ Endpoint,
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"token">>, Access, <<"client.p">>),

         _ < 302,
       Url < "Location: _",
         _ < oauth2_redirect_url(Url),
         _ < oauth2_redirect_url_with_state(Url),
         _ < oauth2_redirect_url_with_ttl(Url),
         _ < oauth2_redirect_url_with_access_token(Url, oauth2_fixtures:access_token(Access))
      ]
   ).


%%
%%
authorization_code_grant_with_confidential_app_signin(_) ->
   {ok, _} = [either ||
      authorization_code_grant_confidential_request("http://localhost:8080/oauth2/signin", <<"account.a">>),
      authorization_code_grant_confidential_access_token(_, <<"account.a">>),
      refresh_grant_confidential(_, <<"account.a">>)
   ].

authorization_code_grant_with_confidential_app_signup(_) ->
   {ok, _} = [either ||
      authorization_code_grant_confidential_request("http://localhost:8080/oauth2/signup", <<"signup.cg.cc.uA">>),
      authorization_code_grant_confidential_access_token(_, <<"signup.cg.cc.uA">>),
      refresh_grant_confidential(_, <<"signup.cg.cc.uA">>)
   ].


authorization_code_grant_confidential_request(Endpoint, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST " ++ Endpoint,
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, Access, <<"client.c">>),

         _ < 302,
       Url < "Location: _",
         _ < oauth2_redirect_url(Url),
         _ < oauth2_redirect_url_with_state(Url),
         _ < oauth2_redirect_url_with_code(Url, oauth2_fixtures:code(Access)),
         _ < oauth2_redirect_url_with_code(Url)
      ]
   ).

authorization_code_grant_confidential_access_token(Code, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > exchange_request(Code, <<"client.c">>),

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token(Access)),
         _ < oauth2_refresh_token(oauth2_fixtures:refresh_token(Access)),
         _ < oauth2_refresh_token()
      ]
   ).


refresh_grant_confidential(Token, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{grant_type => <<"refresh_token">>, refresh_token => Token},

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token(Access)),
         _ < oauth2_refresh_token(oauth2_fixtures:refresh_token(Access))
      ]
   ).

%%
%%
authorization_code_grant_with_invalid_confidential_app_signin(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/signin",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.invalid:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, <<"account.a">>, <<"client.invalid">>),

         _ < 401
      ]
   ).

authorization_code_grant_with_invalid_confidential_app_signup(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/signup",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.invalid:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"code">>, <<"account.a">>, <<"client.invalid">>),

         _ < 401
      ]
   ).

%%
%%
implicit_grant_with_confidential_app_signin(_) ->
   {ok, _} = implicit_grant_confidential_request("http://localhost:8080/oauth2/signin", <<"account.a">>).

implicit_grant_with_confidential_app_signup(_) ->
   {ok, _} = implicit_grant_confidential_request("http://localhost:8080/oauth2/signup", <<"signup.ig.cc.uA">>).

implicit_grant_confidential_request(Endpoint, Access) ->
   m_http:once(
      [m_http ||
         _ > "POST " ++ Endpoint,
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > authorization_request(<<"token">>, Access, <<"client.c">>),

         _ < 302,
       Url < "Location: _",
         _ < oauth2_redirect_url(Url),
         _ < oauth2_redirect_url_with_state(Url),
         _ < oauth2_redirect_url_with_ttl(Url),
         _ < oauth2_redirect_url_with_access_token(Url, oauth2_fixtures:access_token(Access))
      ]
   ).

%%
%%
resource_owner_password_grant_with_public_app(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{grant_type => <<"password">>, username => <<"account.a">>, password => <<"nosecret">>, client_id  => <<"client.p">>},

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token(<<"account.a">>)),
         _ < oauth2_not_defined_refresh_token()
      ]
   ).

%%
%%
resource_owner_password_grant_with_confidential_app(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),         
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{grant_type => <<"password">>, username => <<"account.a">>, password => <<"nosecret">>, client_id  => <<"client.c">>},

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token(<<"account.a">>)),
         _ < oauth2_refresh_token(oauth2_fixtures:refresh_token(<<"account.a">>))
      ]
   ).


%%
%%
client_credentials_grant(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/token",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),         
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{grant_type => <<"client_credentials">>},

         _ < 200,
         _ < "Content-Type: application/json",
         _ < oauth2_access_token(oauth2_fixtures:access_token_client(<<"client.c">>)),
         _ < oauth2_refresh_token(oauth2_fixtures:refresh_token_client(<<"client.c">>))
      ]
   ).

%%
%%
jwks(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://localhost:8080/oauth2/jwks",
         _ > "Accept: */*",
         _ > "Connection: close",

         _ < 200,
         _ < "Content-Type: application/json",
         _ < lens:c(lens:at(<<"keys">>), lens:hd(), lens:at(<<"kid">>), lens:require(<<"jwt">>)),
         _ < lens:c(lens:at(<<"keys">>), lens:hd(), lens:at(<<"kty">>), lens:require(<<"RSA">>)),
         _ < lens:c(lens:at(<<"keys">>), lens:hd(), lens:at(<<"n">>), lens:defined()),
         _ < lens:c(lens:at(<<"keys">>), lens:hd(), lens:at(<<"e">>), lens:defined())
      ]
   ).


%%
%%
introspect(_) ->
   {ok, Token} = oauth2_token:access(<<"account.a">>, <<"nosecret">>),
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/introspect",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),         
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{token => Token},

         _ < 200,
         _ < lens:c(lens:at(<<"active">>), lens:require(true)),
         _ < lens:c(lens:at(<<"iss">>), lens:require(<<"http://localhost:8080">>)),
         _ < lens:c(lens:at(<<"sub">>), lens:require(<<"account.a">>)),
         _ < lens:c(lens:at(<<"uid">>), lens:require(true)),
         _ < lens:c(lens:at(<<"aud">>), lens:require(<<"any">>)),
         _ < lens:c(lens:at(<<"exp">>), lens:defined()),
         _ < lens:c(lens:at(<<"tji">>), lens:defined())
      ]
   ).

%%
%%
introspect_invalid_token(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/introspect",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.c:nosecret">>)),         
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{token => <<"invalid">>},

         _ < 200,
         _ < lens:c(lens:at(<<"active">>), lens:require(false))
      ]
   ).

%%
%%
introspect_invalid_client(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "POST http://localhost:8080/oauth2/introspect",
         _ > "Accept: */*",
         _ > "Connection: close",
         _ > "Authorization: Basic " ++ scalar:c(base64:encode(<<"client.invalid:nosecret">>)),         
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{token => <<"invalid">>},

         _ < 401
      ]
   ).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   


%%
%% data type to issue authorization request
%%
%% https://tools.ietf.org/html/rfc6749#section-4.1.1
%% https://tools.ietf.org/html/rfc6749#section-4.2.1
authorization_request(Type, Access, OAuthApp) ->
   #{
      access         => Access,
      secret         => <<"nosecret">>,
      response_type  => Type,
      client_id      => OAuthApp,
      state          => <<"opaque">>
   }.

%%
%% data type to issue code exchange request
exchange_request(Code, OAuth2App) ->
   #{
      grant_type => <<"authorization_code">>,
      code       => Code,
      client_id  => OAuth2App
   }.


%%
-spec oauth2_redirect_url(_) -> datum:lens().

oauth2_redirect_url(Url) ->
   lens:c(
      lens_const(Url),
      lens_url(), 
      lens:require(<<"http://example.com/path">>)
   ).

%%
-spec oauth2_redirect_url_with_state(_) -> datum:lens().

oauth2_redirect_url_with_state(Url) ->
   lens:c(
      lens_const(Url),
      lens_url_q(),
      lens:pair(<<"state">>), 
      lens:require(<<"opaque">>)
   ).

%%
-spec oauth2_redirect_url_with_code(_) -> datum:lens().

oauth2_redirect_url_with_code(Url) ->
   lens:c(
      lens_const(Url),
      lens_url_q(),
      lens:pair(<<"code">>),
      lens:defined()
   ). 

%%
-spec oauth2_redirect_url_with_code(_, _) -> datum:lens().

oauth2_redirect_url_with_code(Url, Claims) ->
   lens:c(
      lens_const(Url),
      lens_url_q(),
      lens:pair(<<"code">>),
      lens_token(),
      lens:require(Claims)
   ).

%%
-spec oauth2_redirect_url_with_ttl(_) -> datum:lens().

oauth2_redirect_url_with_ttl(Url) ->
   lens:c(
      lens_const(Url),
      lens_url_q(),
      lens:pair(<<"expires_in">>),
      lens:defined()
   ). 

%%
-spec oauth2_redirect_url_with_access_token(_, _) -> datum:lens().

oauth2_redirect_url_with_access_token(Url, Claims) ->
   lens:c(
      lens_const(Url),
      lens_url_q(),
      lens:pair(<<"access_token">>),
      lens_token(),
      lens:require(Claims)
   ).

%%
-spec oauth2_access_token(_) -> datum:lens().

oauth2_access_token(Claims) ->
   lens:c(
      lens_access_token(),
      lens_token(),
      lens:require(Claims)
   ).

%%
-spec oauth2_not_defined_refresh_token() -> datum:lens().

oauth2_not_defined_refresh_token() ->
   lens:c(
      lens:at(<<"refresh_token">>, undefined),
      lens:require(undefined)
   ).

%%
-spec oauth2_refresh_token(_) -> datum:lens().

oauth2_refresh_token(Claims) ->
   lens:c(
      lens_refresh_token(),
      lens_token(),
      lens:require(Claims)
   ).

%%
-spec oauth2_refresh_token() -> datum:lens().

oauth2_refresh_token() ->
   lens:c(
      lens_refresh_token(),
      lens:defined()
   ).   


%%%----------------------------------------------------------------------------   
%%%
%%% lenses
%%%
%%%----------------------------------------------------------------------------   

%%
-spec lens_const(_) -> datum:lens().

lens_const(X) ->
   fun(Fun, _) ->
      lens:fmap(fun(_) -> X end, Fun(X)) 
   end.

%%
-spec lens_uri(_) -> datum:lens().

lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.

%%
-spec lens_url() -> datum:lens().

lens_url() ->
   lens_uri(fun(Uri) -> uri:s(uri:anchor(undefined, uri:q(undefined, Uri))) end).

%%
-spec lens_url_q()-> datum:lens().

lens_url_q() ->
   lens_uri(fun uri:q/1).

%%
-spec lens_token() -> datum:lens().

lens_token() ->
   fun(Fun, Token) ->
      case permit:validate(Token) of
         {ok, #{<<"tji">> := _, <<"exp">> := _} = Claims} -> 
            lens:fmap(
               fun(_) -> Token end, 
               Fun( maps:without([<<"tji">>, <<"exp">>], Claims) )
            );
         {error, Reason} ->
            throw(Reason)
      end
   end.


%%
-spec lens_access_token() -> datum:lens().

lens_access_token() ->
   fun(Fun, #{<<"token_type">> := <<"bearer">>, <<"expires_in">> := _, <<"access_token">> := Token} = Access) ->
      lens:fmap(fun(_) -> Access end, Fun( Token ))
   end.

%%
-spec lens_refresh_token() -> datum:lens().

lens_refresh_token() ->
   fun(Fun, #{<<"token_type">> := <<"bearer">>, <<"refresh_token">> := Token} = Refresh) ->
      lens:fmap(fun(_) -> Refresh end, Fun( Token ))
   end.

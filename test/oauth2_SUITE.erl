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
   authorize/1,
   authorization_code_grant_with_public_app_signin/1,
   authorization_code_grant_with_public_app_signup/1,
   implicit_grant_with_public_app_signin/1,
   implicit_grant_with_public_app_signup/1,

   authorization_code_grant_with_confidential_app_signin/1,
   authorization_code_grant_with_confidential_app_signup/1,
   implicit_grant_with_confidential_app_signin/1,
   implicit_grant_with_confidential_app_signup/1,

   resource_owner_password_grant_with_public_app/1,
   resource_owner_password_grant_with_confidential_app/1,

   client_credentials_grant/1,

   jwks/1,
   introspect/1
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
            authorize,

            authorization_code_grant_with_public_app_signin,
            authorization_code_grant_with_public_app_signup,
            implicit_grant_with_public_app_signin,
            implicit_grant_with_public_app_signup,

            authorization_code_grant_with_confidential_app_signin,
            authorization_code_grant_with_confidential_app_signup,
            implicit_grant_with_confidential_app_signin,
            implicit_grant_with_confidential_app_signup,

            resource_owner_password_grant_with_public_app,
            resource_owner_password_grant_with_confidential_app,

            client_credentials_grant
       ]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

%% OAuth2 Authorization Server endpoints
url_auth()   -> "http://localhost:8080/oauth2/authorize".
url_signin() -> "http://localhost:8080/oauth2/signin".
url_signup() -> "http://localhost:8080/oauth2/signup".
url_token()  -> "http://localhost:8080/oauth2/token".
url_jwks()   -> "http://localhost:8080/oauth2/jwks".
url_introspect() -> "http://localhost:8080/oauth2/introspect".


init_per_suite(Config) ->
   oauth2:start(),
   {ok, _} = define_account(),
   {ok, _} = define_client_public(),
   {ok, _} = define_client_confidential(),
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
   define_client(<<"client.p">>, <<"nosecret">>,
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"http://example.com/path">>,
         <<"master">> => <<"account.a">>
      }
   ).

%%
define_client_confidential() ->
   define_client(<<"client.c">>, <<"nosecret">>, 
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"confidential">>,
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

%%
define_account() ->
   oauth2_account:create(<<"account.a">>, <<"nosecret">>,
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

%%
%%
authorize(_) ->
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_auth() ),

         cats:with(),
         cats:x('GET'),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() )
      ]
   ).

%%
%%
authorization_code_grant_with_public_app_signin(_) ->
   {ok, _} = [either ||
      authorization_code_grant_public_request(url_signin(), <<"account.a">>),
      authorization_code_grant_public_access_token(_, <<"account.a">>)
   ].

authorization_code_grant_with_public_app_signup(_) ->
   {ok, _} = [either ||
      authorization_code_grant_public_request(url_signup(), <<"signup.cg.pc.uA">>),
      authorization_code_grant_public_access_token(_, <<"signup.cg.pc.uA">>)
   ].

authorization_code_grant_public_request(Url, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( Url ),
         cats:payload( authorization_request(<<"code">>, Access, <<"client.p">>) ),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_302() ),
         cats:require( oauth2_redirect_url() ),
         cats:require( oauth2_redirect_url_with_state() ),
         cats:require( oauth2_redirect_url_with_code(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"exch">>=> true
         }) ),
         cats:require( oauth2_redirect_url_with_code() )
      ]
   ).

authorization_code_grant_public_access_token(Code, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"authorization_code">>,
            code       => Code,
            client_id  => <<"client.p">>
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_not_defined_refresh_token() )
      ]
   ).


%%
%%
implicit_grant_with_public_app_signin(_) ->
   {ok, _} = implicit_grant_public_request(url_signin(), <<"account.a">>).

implicit_grant_with_public_app_signup(_) ->
   {ok, _} = implicit_grant_public_request(url_signup(), <<"signup.ig.pc.uA">>).

implicit_grant_public_request(Url, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( Url ),
         cats:payload( authorization_request(<<"token">>, Access, <<"client.p">>) ),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_302() ),
         cats:require( oauth2_redirect_url() ),
         cats:require( oauth2_redirect_url_with_state() ),
         cats:require( oauth2_redirect_url_with_ttl() ),
         cats:require( oauth2_redirect_url_with_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"uid">> => true
         }) )
      ]
   ).


%%
%%
authorization_code_grant_with_confidential_app_signin(_) ->
   {ok, _} = [either ||
      authorization_code_grant_confidential_request(url_signin(), <<"account.a">>),
      authorization_code_grant_confidential_access_token(_, <<"account.a">>),
      refresh_grant_confidential(_, <<"account.a">>)
   ].

authorization_code_grant_with_confidential_app_signup(_) ->
   {ok, _} = [either ||
      authorization_code_grant_confidential_request(url_signup(), <<"signup.cg.cc.uA">>),
      authorization_code_grant_confidential_access_token(_, <<"signup.cg.cc.uA">>),
      refresh_grant_confidential(_, <<"signup.cg.cc.uA">>)
   ].


authorization_code_grant_confidential_request(Url, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( Url ),
         cats:payload( authorization_request(<<"code">>, Access, <<"client.c">>) ),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_302() ),
         cats:require( oauth2_redirect_url() ),
         cats:require( oauth2_redirect_url_with_state() ),
         cats:require( oauth2_redirect_url_with_code(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"exch">>=> true
         }) ),
         cats:require( oauth2_redirect_url_with_code() )
      ]
   ).

authorization_code_grant_confidential_access_token(Code, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"authorization_code">>,
            code       => Code,
            client_id  => <<"client.c">>
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_refresh_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,            
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"exch">> => true
         })),
         cats:require( oauth2_refresh_token() )
      ]
   ).


refresh_grant_confidential(Token, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"refresh_token">>,
            refresh_token => Token
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_refresh_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,            
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"exch">> => true
         }))
      ]
   ).

%%
%%
implicit_grant_with_confidential_app_signin(_) ->
   {ok, _} = implicit_grant_confidential_request(url_signin(), <<"account.a">>).

implicit_grant_with_confidential_app_signup(_) ->
   {ok, _} = implicit_grant_confidential_request(url_signup(), <<"signup.ig.cc.uA">>).

implicit_grant_confidential_request(Url, Access) ->
   kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( Url ),
         cats:payload( authorization_request(<<"token">>, Access, <<"client.c">>) ),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),         
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_302() ),
         cats:require( oauth2_redirect_url() ),
         cats:require( oauth2_redirect_url_with_state() ),
         cats:require( oauth2_redirect_url_with_ttl() ),
         cats:require( oauth2_redirect_url_with_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => Access,
            <<"rev">> => true,
            <<"uid">> => true
         }) )
      ]
   ).

%%
%%
resource_owner_password_grant_with_public_app(_) ->
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"password">>,
            username   => <<"account.a">>,
            password   => <<"nosecret">>,
            client_id  => <<"client.p">>
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => <<"account.a">>,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_not_defined_refresh_token() )
      ]
   ).

%%
%%
resource_owner_password_grant_with_confidential_app(_) ->
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"password">>,
            username   => <<"account.a">>,
            password   => <<"nosecret">>,
            client_id  => <<"client.p">>
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"sub">> => <<"account.a">>,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_refresh_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,            
            <<"sub">> => <<"account.a">>,
            <<"rev">> => true,
            <<"exch">> => true
         }))
      ]
   ).


%%
%%
client_credentials_grant(_) ->
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_token() ),
         cats:payload(#{
            grant_type => <<"client_credentials">>
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() ),
         cats:require( oauth2_access_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"idp">> => <<"account.a">>,
            <<"sub">> => <<"client.c">>,
            <<"rev">> => true,
            <<"uid">> => true
         }) ),
         cats:require( oauth2_refresh_token(#{
            <<"iss">> => <<"http://localhost:8080">>,
            <<"aud">> => <<"any">>,
            <<"idp">> => <<"account.a">>,
            <<"sub">> => <<"client.c">>,
            <<"rev">> => true,
            <<"exch">> => true
         }))

      ]
   ).

%%
%%
jwks(_) ->
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_jwks() ),

         cats:with(),
         cats:x('GET'),
         cats:h("Accept: */*"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() )
         % cats:require(json(#{
         %    <<"keys">> => [
         %       #{
         %          <<"kid">> := <<"jwt">>,
         %          <<"alg">> := <<"RS256">>,
         %          <<"kty">> := <<"RSA">>,
         %          <<"n">>   := _,
         %          <<"e">>   := _
         %       }
         %    ]
         % }))
      ]
   ).


%%
%%
introspect(_) ->
   {ok, Token} = oauth2_token:access(<<"account.a">>, <<"nosecret">>),
   {ok, _} = kscript:once(
      [m_knet ||
         cats:given(),
         cats:url( url_introspect() ),
         cats:payload(#{
            token => Token
         }),

         cats:with(),
         cats:x('POST'),
         cats:h("Accept: */*"),
         cats:h("Authorization", <<"Basic ", (base64:encode(<<"client.c:nosecret">>))/binary>>),
         cats:h("Content-Type: application/x-www-form-urlencoded"),
         cats:h("Connection: close"),

         cats:then(),
         cats:require( http_200() )
         % cats:require( json(#{
         %    <<"iss">> := <<"http://localhost:8080">>,
         %    <<"sub">> := <<"account.a">>,
         %    <<"uid">> := true,
         %    <<"aud">> := <<"any">>,
         %    <<"exp">> := _,
         %    <<"tji">> := _
         % }))
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
%% pattern-match: 
%%   redirect header to valid url
oauth2_redirect_url() ->
   lens:c(
      lens:at(headers),  
      lens:pair(<<"Location">>), 
      lens_url(), 
      lens_require(<<"http://example.com/path">>)
   ).

%%
%% pattern-match:
%%   opaque oauth2 app state is handled by server
oauth2_redirect_url_with_state() ->
   lens:c(
      lens:at(headers), 
      lens:pair(<<"Location">>),
      lens_url_q(), 
      lens:pair(<<"state">>), 
      lens_require(<<"opaque">>)
   ).

%%
%% pattern-match:
%%   presence of authorization code
oauth2_redirect_url_with_code() ->
   lens:c(
      lens:at(headers), 
      lens:pair(<<"Location">>),
      lens_url_q(), 
      lens:pair(<<"code">>),
      lens_defined()
   ). 

%%
%% pattern-match:
%%   authorization code is valid and contains required claims
oauth2_redirect_url_with_code(Claims) ->
   lens:c(
      lens:at(headers), 
      lens:pair(<<"Location">>),
      lens_url_q(), 
      lens:pair(<<"code">>),
      lens_token(),
      lens_require(Claims)
   ).

%%
%% pattern-match:
%%   access token is valid and contains required claims
oauth2_redirect_url_with_access_token(Claims) ->
   lens:c(
      lens:at(headers), 
      lens:pair(<<"Location">>),
      lens_url_q(), 
      lens:pair(<<"access_token">>),
      lens_token(),
      lens_require(Claims)
   ).

oauth2_redirect_url_with_ttl() ->
   lens:c(
      lens:at(headers), 
      lens:pair(<<"Location">>),
      lens_url_q(), 
      lens:pair(<<"expires_in">>),
      lens_defined()
   ).

%%
%% pattern-match:
%%   access token is valid and contains require claims
oauth2_access_token(Claims) ->
   lens:c(
      lens:at(content),
      lens_access_token(),
      lens_token(),
      lens_require(Claims)
   ).

%%
%% pattern-match:
%%   refresh token is valid and contains require claims
oauth2_refresh_token(Claims) ->
   lens:c(
      lens:at(content),
      lens_refresh_token(),
      lens_token(),
      lens_require(Claims)
   ).

oauth2_refresh_token() ->
   lens:c(
      lens:at(content),
      lens_refresh_token(),
      lens_defined()
   ).

%%
%% pattern-match:
%%   refresh token is not defined
oauth2_not_defined_refresh_token() ->
   lens:c(
      lens:at(content),
      lens:at(<<"refresh_token">>, undefined),
      lens_require(undefined)
   ).



http_302() ->
   lens:c(
      lens:at(status), 
      lens_require(302) 
   ).

http_200() ->
   lens:c(
      lens:at(status), 
      lens_require(200) 
   ).

json(Json) ->
   lens:c(
      lens:at(content),
      lens_require(Json)
   ).


%%
%% lenses implements a either semantic required by kscript 
%%
lens_require(Code) ->
   fun(Fun, X) ->
      case X of
         Code ->
            lens:fmap(fun(_) -> X end, Fun({ok, Code}));
         _    ->
            lens:fmap(fun(_) -> X end, Fun({error, {require, Code, X}}))
      end
   end.

lens_defined() ->
   fun(Fun, X) ->
      lens:fmap(fun(_) -> X end, Fun({ok, X}))
   end.

%%
%%
lens_url() ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( decode_url(Uri) ))
   end.

decode_url(Uri) ->
   [identity ||
      uri:new(Uri),
      uri:q(undefined, _),
      uri:anchor(undefined, _),
      uri:s(_)
   ].

%%
%%
lens_url_q() ->
   lens_uri(fun uri:q/1).

%%
%%
lens_token() ->
   fun(Fun, Token) ->
      lens:fmap(fun(_) -> Token end, Fun( decode_token(Token) ))
   end.

decode_token(Token) ->
   case permit:validate(Token) of
      {ok, #{<<"tji">> := _, <<"exp">> := _} = Claims} -> 
         maps:without([<<"tji">>, <<"exp">>], Claims);
      {error, Reason} ->
         throw(Reason)
   end.

%%
%%
lens_access_token() ->
   fun(Fun, Token) ->
      lens:fmap(fun(_) -> Token end, Fun( decode_access_token(Token) ))
   end.

decode_access_token(#{<<"token_type">> := <<"bearer">>, <<"expires_in">> := _, <<"access_token">> := Token}) ->
   Token.

%%
%%
lens_refresh_token() ->
   fun(Fun, Token) ->
      lens:fmap(fun(_) -> Token end, Fun( decode_refresh_token(Token) ))
   end.

decode_refresh_token(#{<<"token_type">> := <<"bearer">>, <<"refresh_token">> := Token}) ->
   Token.



lens_uri(Accessor) ->
   fun(Fun, Uri) ->
      lens:fmap(fun(_) -> Uri end, Fun( Accessor(uri:new(Uri)) ))
   end.


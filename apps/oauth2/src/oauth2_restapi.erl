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
%%   common restapi utility
-module(oauth2_restapi).
-compile({parse_transform, category}).

-include("oauth2.hrl").

-export([endpoints/0]).

%%
%%
endpoints() ->
   [
      %% https://tools.ietf.org/html/rfc6749
      confidential_client_signin(),
      confidential_client_signup(),
      confidential_client_access_token(),

      public_client_signin(),
      public_client_signup(),
      public_client_access_token(),

      %% 
      introspect(),
      jwks(),

      client_create(),
      client_remove(),
      client_lookup(),

      restd_static:react("/oauth2/authorize", oauth2, 'oauth2-signin'),
      restd_static:react("/oauth2/account",   oauth2, 'oauth2-account')
   ].


%%
%% The authorization code grant type is used to obtain both access
%% tokens and refresh tokens and is optimized for confidential clients.
%%
%% https://tools.ietf.org/html/rfc6749
%%   Section 4.1.1.  Authorization Request
%%
confidential_client_signin() ->
   [reader ||
      _ /= restd:path("/oauth2/signin"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),

      Digest  /= restd:header(<<"Authorization">>),
      Client  <- authenticate_http_digest(Digest),
      Request /= restd:as_form(),

      oauth2:signin(Request, Client),

      Http /= restd:to_text(redirect, [{<<"Location">>, _}], <<$ >>),
      _ /= restd:accesslog(Http)
   ].

confidential_client_signup() ->
   [reader ||
      _ /= restd:path("/oauth2/signup"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),

      Digest  /= restd:header(<<"Authorization">>),
      Client  <- authenticate_http_digest(Digest),
      Request /= restd:as_form(),

      oauth2:signup(Request, Client),

      Http /= restd:to_text(redirect, [{<<"Location">>, _}], <<$ >>),
      _ /= restd:accesslog(Http)
   ].



%%
%% The implicit grant type is used to obtain access tokens (it does not
%% support the issuance of refresh tokens) and is optimized for public
%% clients known to operate a particular redirection URI.  These clients
%% are typically implemented in a browser using a scripting language
%% such as JavaScript.
%%
%% https://tools.ietf.org/html/rfc6749
%%   Section 4.2.1.  Authorization Request
%%
public_client_signin() ->
   [reader ||
      _ /= restd:path("/oauth2/signin"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),

      Request /= restd:as_form(),
      Client  <- authenticate_public_client(Request),

      oauth2:signin(Request, Client),

      Http /= restd:to_text(redirect, [{<<"Location">>, _}], <<$ >>),
      _ /= restd:accesslog(Http)
   ].


public_client_signup() ->
   [reader ||
      _ /= restd:path("/oauth2/signup"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),

      Request /= restd:as_form(),
      Client  <- authenticate_public_client(Request),

      oauth2:signup(Request, Client),

      Http /= restd:to_text(redirect, [{<<"Location">>, _}], <<$ >>),
      _ /= restd:accesslog(Http)
   ].


%%
%% https://tools.ietf.org/html/rfc6749
%%   Section 4.1.3. Access Token Request (Authorization Code Grant)
%%   Section 4.3.2. Access Token Request (Resource Owner Password Credentials Grant)
%%   Section 4.4.2. Access Token Request (Client Credentials Grant)
%%   Section 6. Refreshing an Access Token
%%
confidential_client_access_token() ->
   [reader ||
      _ /= restd:path("/oauth2/token"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),
      _ /= restd:provided_content({application, json}),

      Digest  /= restd:header(<<"Authorization">>),
      Client  <- authenticate_http_digest(Digest),
      Request /= restd:as_form(),

      cats:unit(oauth2:token(Request, Client)),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].

%%
%% https://tools.ietf.org/html/rfc6749
%%   Section 4.1.3. Access Token Request (Authorization Code Grant)
%%   Section 4.3.2. Access Token Request (Resource Owner Password Credentials Grant)
%%   Section 4.4.2. Access Token Request (Client Credentials Grant)
%%   Section 6. Refreshing an Access Token
%%
public_client_access_token() ->
   [reader ||
      _ /= restd:path("/oauth2/token"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),
      _ /= restd:provided_content({application, json}),

      Request /= restd:as_form(),
      Client  <- authenticate_public_client(Request),

      cats:unit(oauth2:token(Request, Client)),

      Http /= restd:to_json(_), 
      _ /= restd:accesslog(Http)
   ].


%%
%% https://tools.ietf.org/html/rfc7662
%%   Section 2.1. Introspection Request
%%   To prevent token scanning attacks, the endpoint MUST also require
%%   some form of authorization to access this endpoint.
%%   (The end-point is only available for confidential clients)
%%
introspect() ->
   [reader ||
      _ /= restd:path("/oauth2/introspect"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, 'x-www-form-urlencoded'}),
      _ /= restd:provided_content({application, json}),

      Digest  /= restd:header(<<"Authorization">>),
           _  <- authenticate_http_digest(Digest),
      Request /= restd:as_form(),

      cats:optionT(badarg, lens:get(lens:at(<<"token">>, undefined), Request)),
      cats:unit(permit:validate(_)),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].

%%
%%
jwks() ->
   [reader ||
      _ /= restd:path("/oauth2/jwks"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),

      permit_config:public(),
      jwk:encode(<<"jwt">>, _),

      Http <- cats:unit({200, [{<<"Content-Type">>, <<"application/json">>}], _}), 
      _ /= restd:accesslog(Http)
   ].

%%
%%
client_create() ->
   [reader ||
      _ /= restd:path("/oauth2/client"),
      _ /= restd:method('POST'),
      _ /= restd:provided_content({application, json}),

      Token /= restd:header(<<"Authorization">>),
      Jwt   <- authenticate_access_token(Token),
      Json  /= restd:as_json(),
      cats:unit(oauth2_client:create(Jwt, Json)),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].


client_remove() ->
   [reader ||
      Path /= restd:path("/oauth2/client/_"),
      _ /= restd:method('DELETE'),
      _ /= restd:provided_content({application, json}),

      Token /= restd:header(<<"Authorization">>),
      Jwt   <- authenticate_access_token(Token),
      Id    <- client_identity(Path),
      cats:unit(oauth2_client:remove(Jwt, Id)),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].

client_lookup() ->
   [reader ||
      _ /= restd:path("/oauth2/client"),
      _ /= restd:method('GET'),
      _ /= restd:provided_content({application, json}),

      Token /= restd:header(<<"Authorization">>),
      Jwt   <- authenticate_access_token(Token),
      cats:unit(oauth2_account:apps(Jwt)),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].

client_identity([_, _, Id]) ->
   {ok, Id};
client_identity(_) ->
   {error, badarg}.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
authenticate_http_digest(<<"Basic ", Digest/binary>>) ->
   [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
   [either ||
      permit:stateless(Access, Secret, 1, #{}),
      Client <- oauth2_client:lookup(Access),
      oauth2_client:is_confidential(Client),
      Token  <- oauth2_token:exchange_code(Access, Secret),
      %% how to pass client access / secret so that bearer token is allocatable
      unit(Client#{<<"identity">> => Token})
   ];
authenticate_http_digest(_) ->
   {error, unauthorized_client}.

%%
authenticate_public_client(#{<<"client_id">> := Access}) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:is_public(_)
   ];
authenticate_public_client(_) ->
   {error, badarg}.   

%%
authenticate_access_token(<<"Bearer ", Token/binary>>) ->
   permit:validate(Token);
authenticate_access_token(_) ->
   {ok, unauthorized}.


% -export([
%    decode/1,        %% @deprecated use restd:as_form
%    authenticate/2,
%    access_token/1
% ]).


% %%
% %% decodes oauth2 request
% %% parse application/x-www-form-urlencoded to map
% -spec decode(_) -> {ok, _} | {error, _}.  

% decode(Request) ->
%    {ok, [$. ||
%       binary:split(scalar:s(Request), <<$&>>, [trim, global]),
%       lists:map(fun as_pair/1, _),
%       maps:from_list(_)
%    ]}.

% as_pair(Pair) ->
%    erlang:list_to_tuple(
%       [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
%    ).


% %%
% %% authenticate client and injects its identity into environment
% -spec authenticate(_, _) -> ok | {error, _}.

% authenticate(#{<<"client_id">> := Access} = Env, Head) ->
%    [either ||
%       oauth2_client:lookup(Access),
%       authenticate_client(_, Env, Head)
%    ];

% authenticate(Env, Head) ->
%    [either ||
%       category:optionT(unauthorized_client,
%          lens:get( lens:pair('Authorization', undefined), Head )
%       ),
%       authenticate_http_digest(_, Env)
%    ].

% %%
% authenticate_client(#{<<"security">> := <<"public">>}, Env, _) ->
%    {ok, Env};
% authenticate_client(#{<<"security">> := <<"confidential">>}, Env, Head) ->
%    [either ||
%       category:optionT(unauthorized_client,
%          lens:get( lens:pair('Authorization', undefined), Head )
%       ),
%       authenticate_http_digest(_, Env)
%    ].

% %%
% authenticate_http_digest(<<"Basic ", Digest/binary>>, Env) ->
%    [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
%    [either ||
%       permit:stateless(Access, Secret, 1, #{}),
%       oauth2_client:lookup(Access),
%       oauth2_client:is_confidential(_),
%       unit(Env#{<<"client_id">> => Access, <<"client_secret">> => Secret})
%    ];
% authenticate_http_digest(_, _) ->
%    {error, unauthorized_client}.


% %%
% %%
% access_token(Head) ->
%    case lens:get(lens:pair('Authorization', undefined), Head) of
%       <<"Bearer ", Token/binary>> ->
%          Token;
%       _ ->
%          undefined
%    end.


%%
%% @doc
%%   authorization request
%%     https://tools.ietf.org/html/rfc6749#section-4.1.1
%%     https://tools.ietf.org/html/rfc6749#section-4.2.1
-module(oauth2_authorize).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").

-export([
   signup/2
,  signin/2
,  exchange_code/2
]).

%%
%%
-spec signup(#{}, binary()) -> datum:either(uri:uri()).

signup(Headers, Request)
 when is_binary(Request) ->
   req_signup_auth(Headers, lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request))).

%%
req_signup_auth(#{<<"Authorization">> := Digest}, Request) ->
   [either ||
      #{
         <<"client_id">>    := ClientId,
         <<"redirect_uri">> := Redirect
      } <- oauth2_client:confidential(Digest),
      req_signup(uri:new(Redirect), Request#authorization{client_id = ClientId})
   ];

req_signup_auth(_, #authorization{client_id = Client} = Request) ->
   [either ||
      #{<<"redirect_uri">> := Redirect} <- oauth2_client:public(Client),
      req_signup(uri:new(Redirect), Request)
   ].

%%
req_signup(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:create(Access, Secret, Claims),
         exchange_code(_, Claims)
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signup(Redirect, #authorization{
   response_type = <<"token">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State  
}) ->
   case
      [either ||
         permit:create(Access, Secret, Claims),
         permit:revocable(_, 3600, Claims) %% TODO: configurable ttl
      ]
   of
      {ok, Token} ->
         {ok, uri:q([{access_token, Token}, {expires_in, 3600}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signup(_, _) ->
   {error, invalid_request}.

%%
%%
-spec signin(#{}, binary()) -> datum:either(uri:uri()).

signin(Headers, Request)
 when is_binary(Request) ->
   req_signin_auth(Headers, lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request))).

%%
req_signin_auth(#{<<"Authorization">> := Digest}, Request) ->
   [either ||
      #{
         <<"client_id">>    := ClientId,
         <<"redirect_uri">> := Redirect
      } <- oauth2_client:confidential(Digest),
      req_signin(uri:new(Redirect), Request#authorization{client_id = ClientId})
   ];

req_signin_auth(_, #authorization{client_id = Client} = Request) ->
   [either ||
      #{<<"redirect_uri">> := Redirect} <- oauth2_client:public(Client),
      req_signin(uri:new(Redirect), Request)
   ].

req_signin(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:stateless(Access, Secret, 3600, Claims),
         exchange_code(_, Claims)
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signin(Redirect, #authorization{
   response_type = <<"token">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      permit:revocable(Access, Secret, 3600, Claims) %% TODO: configurable ttl
   of
      {ok, Token} ->
         {ok, uri:q([{access_token, Token}, {expires_in, 3600}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signin(_, _) ->
   {error, invalid_request}.

%%
%%
exchange_code(Token, Claims) ->
   %% TODO: configurable ttl
   permit:stateless(Token, 3600, #{
      <<"aud">> => <<"oauth2">>
   ,  <<"app">> => base64url:encode(jsx:encode(Claims))
   }).

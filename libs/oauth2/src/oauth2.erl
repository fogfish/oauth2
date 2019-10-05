-module(oauth2).

-compile({parse_transform, category}).
-include_lib("oauth2/include/oauth2.hrl").

-export([
   auth_client_public/1
,  auth_client_confidential/1
,  signup/1
,  signin/1
]).

-type digest() :: binary().

%%
%%
-spec auth_client_public(digest()) -> datum:either(permit:claims()).

auth_client_public(<<"account@oauth2">>) ->
   %% Note: account@oauth2 is built-in expereince
   %%       the not_found fallback simplifies management of clients registrations
   case auth_client_public({iri, <<"oauth2">>, <<"account">>}) of
      {error, not_found} ->
         auth_client_public_default();
      Result ->
         Result
   end;

auth_client_public(Access)
 when is_binary(Access) ->
   [either || iri(Access), auth_client_public(_)];

auth_client_public({iri, _, _} = Access) ->
   [either ||
      permit:lookup(Access),
      permit:include(_, #{<<"security">> => <<"public">>})
   ].

auth_client_public_default() ->
   [either ||
      Spec =< #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => uri:s(uri:path("/oauth2/account", uri:new(permit_config:iss())))
      },
      permit:create({iri, <<"oauth2">>, <<"account">>}, crypto:strong_rand_bytes(30), Spec),
      cats:unit(Spec)
   ].

%%
%%
-spec auth_client_confidential(digest()) -> datum:either(permit:claims()).

auth_client_confidential(<<"Basic ", Digest/binary>>) ->
   [either ||
      [Access, Secret] <- cats:unit(binary:split(base64:decode(Digest), <<$:>>)),
      Identity <- iri(Access),
      permit:stateless(Identity, Secret, 1, #{}),
      permit:lookup(Identity),
      permit:include(_, #{<<"security">> => <<"confidential">>})
   ].

%%
%%
-spec signup(binary() | #authorization{}) -> datum:either(uri:uri()).

signup(Request)
 when is_binary(Request) ->
   signup(lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request)));

signup(#authorization{client_id = {iri, _, _} = Client} = Request) ->
   [either ||
      permit:lookup(Client),
      #{<<"redirect_uri">> := Redirect} <- permit:include(_, #{}),
      signup(uri:new(Redirect), Request)
   ].

signup(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:create(Access, Secret, Claims),
         permit:stateless(_, 3600, #{}) %% TODO: configurable ttl
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

signup(Redirect, #authorization{
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
   end.

%%
%%
-spec signin(binary() | #authorization{}) -> datum:either(uri:uri()).

signin(Request)
 when is_binary(Request) ->
   signin(lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request)));

signin(#authorization{client_id = {iri, _, _} = Client} = Request) ->
   [either ||
      permit:lookup(Client),
      #{<<"redirect_uri">> := Redirect} <- permit:include(_, #{}),
      signin(uri:new(Redirect), Request)
   ].

signin(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:stateless(Access, Secret, 3600, Claims), 
         permit:stateless(_, 3600, #{}) %% TODO: configurable ttl
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

signin(Redirect, #authorization{
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
   end.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%%
iri(Access) ->
   case binary:split(Access, <<$@>>) of
      [Suffix, Prefix] ->
         {ok, {iri, Prefix, Suffix}};
      _ ->
         {error, {badarg, Access}}
   end.

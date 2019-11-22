%% @doc
%%   Client Authentication
%%   https://tools.ietf.org/html/rfc6749#section-2.3
-module(oauth2_client).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").
-include_lib("permit/src/permit.hrl").

-export([
   public/1
,  confidential/1
,  create/2
,  lookup/1
,  remove/2
]).

%%
%% data types
-type digest() :: binary().

-define(CLAIMS,  [<<"app">>, <<"redirect_uri">>, <<"security">>]).

%%
%%
-spec public(digest()) -> datum:either(permit:claims()).

public(Access)
 when is_binary(Access) ->
   [either || permit:to_access(Access), public(_)];

public({iri, _, <<"account@oauth2">>} = Access) ->
   %% Note: account@oauth2 is built-in expereince
   %%       the not_found fallback simplifies management of clients registrations
   case 
      [either ||
         permit:lookup(Access),
         permit:include(_, #{<<"security">> => <<"public">>})
      ]
   of
      {error, not_found} ->
         public_default();
      Result ->
         Result
   end;

public({iri, _, _} = Access) ->
   [either ||
      permit:lookup(Access),
      permit:include(_, #{<<"security">> => <<"public">>})
   ].

public_default() ->
   [either ||
      Spec =< #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => uri:s(uri:path("/oauth2/account", uri:new(permit_config:iss())))
      },
      permit:to_access(<<"account@oauth2">>),
      permit:create(_, crypto:strong_rand_bytes(30), Spec),
      cats:unit(Spec)
   ].

%%
%%
-spec confidential(digest()) -> datum:either(permit:claims()).

confidential(<<"Basic ", Digest/binary>>) ->
   [either ||
      [Access, Secret] <- cats:unit(binary:split(base64:decode(Digest), <<$:>>)),
      Identity <- permit:to_access(Access),
      #pubkey{claims = Claims} = Spec <- permit:lookup(Identity),
      Stateless <- permit:stateless(Identity, Secret, 10, Claims),
      permit:include(Spec, #{<<"security">> => <<"confidential">>}),
      cats:unit(_#{
         <<"client_id">>  => Identity
      ,  <<"client_jwt">> => Stateless
      })
   ].

%%
create(Jwt, Claims) ->
   [either ||
      #{<<"sub">> := Master} <- permit:validate(Jwt),
      Spec <- claims(Claims),
      {Access, Secret} <- permit:pubkey(Master, Spec),
      permit:as_access(Access),
      cats:unit(#{access => _, secret => Secret})
   ].

%%
claims(Claims) ->
   [either ||
      cats:unit(maps:with(?CLAIMS, Claims)),
      claims_security(_),
      claims_redirect_uri(_)
   ].

claims_security(#{<<"security">> := <<"public">>} = Claims) ->
   {ok, Claims};
claims_security(#{<<"security">> := <<"confidential">>} = Claims) ->
   {ok, Claims};
claims_security(_) ->
   {error, invalid_security_profile}.

claims_redirect_uri(#{<<"redirect_uri">> := Uri} = Profile) ->
   [either ||
      cats:unit( uri:new(Uri) ),
      is_some(fun uri:schema/1, _),
      is_some(fun uri:authority/1, _),
      is_some(fun uri:path/1, _),
      is_none(fun uri:q/1, _),
      is_none(fun uri:anchor/1, _),
      cats:unit(Profile)
   ].

is_some(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {error, invalid_uri};
      {<<>>, undefined} ->
         {error, invalid_uri};
      _ ->
         {ok, Uri}
   end.

is_none(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {ok, Uri};
      _ ->
         {error, invalid_uri}
   end.

%%
lookup(Jwt) ->
   [either ||
      #{<<"sub">> := Master} <- permit:validate(Jwt),
      permit_pubkey_db:keys(Master),
      cats:unit(encode_clients(_))
   ].

encode_clients({PubKeys, _}) ->
   [  Claim#{<<"access">> => erlang:element(2, permit:as_access(Access))}
      || #pubkey{
         id = Access,
         claims = #{
            <<"security">>     := _
         ,  <<"redirect_uri">> := _
         } = Claim
      } <- PubKeys
   ].

%%
remove(Jwt, Client) ->
   [either ||
      {iri, IDP, _} = Access <- permit:to_access(Client),
      #{<<"sub">> := {iri, IDP, _} = Master} <- permit:validate(Jwt),
      permit:revoke(Access),
      cats:unit(#{access => Client})
   ].

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
]).

%%
%% data types
-type digest() :: binary().

%%
%%
-spec public(digest()) -> datum:either(permit:claims()).

public(Access)
 when is_binary(Access) ->
   [either || iri(Access), public(_)];

public({iri, <<"oauth2">>, <<"account">>} = Access) ->
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
      permit:create({iri, <<"oauth2">>, <<"account">>}, crypto:strong_rand_bytes(30), Spec),
      cats:unit(Spec)
   ].

%%
%%
-spec confidential(digest()) -> datum:either(permit:claims()).

confidential(<<"Basic ", Digest/binary>>) ->
   [either ||
      [Access, Secret] <- cats:unit(binary:split(base64:decode(Digest), <<$:>>)),
      Identity <- iri(Access),
      #pubkey{claims = Claims} = Spec <- permit:lookup(Identity),
      Stateless <- permit:stateless(Identity, Secret, 10, Claims),
      permit:include(Spec, #{<<"security">> => <<"confidential">>}),
      cats:unit(_#{
         <<"client_id">>  => Identity
      ,  <<"client_jwt">> => Stateless
      })
   ].

%%
%%
iri(Access) ->
   case binary:split(Access, <<$@>>) of
      [Suffix, Prefix] ->
         {ok, {iri, Prefix, Suffix}};
      _ ->
         {error, {badarg, Access}}
   end.

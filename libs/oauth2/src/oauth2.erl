-module(oauth2).

-compile({parse_transform, category}).

-export([
   auth_client_public/1
,  auth_client_confidential/1
]).

-type digest() :: binary().

%%
%%
-spec auth_client_public(digest()) -> datum:either(permit:claims()).

auth_client_public(<<"account@oauth2">>) ->
   %% Note: console@oauth2 is built-in expereince
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
   
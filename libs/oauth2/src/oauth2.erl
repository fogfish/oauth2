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

auth_client_public(Access) ->
   [either ||
      iri(Access),
      permit:lookup(_),
      permit:include(_, #{<<"security">> => <<"public">>})
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
   case binary:split(Access, <<$.>>) of
      [Prefix, Suffix] ->
         {ok, {iri, Prefix, Suffix}};
      _ ->
         {error, {badarg, Access}}
   end.
   
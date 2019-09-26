-module(oauth2).

-compile({parse_transform, category}).

-export([
   auth_public_client/1
]).

-type digest() :: binary().

%%
%%
-spec auth_public_client(digest()) -> datum:either(permit:claims()).

auth_public_client(ClientId) ->
   [either ||
      iri(ClientId),
      permit:lookup(_),
      permit:include(_, #{<<"security">> => <<"public">>})
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
   
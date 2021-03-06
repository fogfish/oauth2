-module(oauth2_codec).

-include_lib("include/oauth2.hrl").
-compile({parse_transform, category}).
-compile({parse_transform, generic}).

-export([
   decode/1
,  authorization/0
,  access_token/0
]).

%%
%%
-spec decode(binary()) -> #{}.

decode(Request) ->
   [identity ||
      binary:split(Request, <<$&>>, [trim, global]),
      lists:map(fun as_pair/1, _),
      maps:from_list(_)
   ].

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).

%%
%%
-spec authorization() -> lens:lens().

authorization() ->
   labelled:lens(
      #authorization{
         client_id = iri()
      ,  scope     = scope()
      ,  access    = iri()
      }
   ).

%%
%%
-spec access_token() -> lens:lens().

access_token() ->
   labelled:lens(
      #access_token{
         client_id = iri()
      ,  username  = iri()
      ,  scope     = scope()
      }
   ).

%%
iri() ->
   fun(Fun, IRI) ->
      lens:fmap(fun(X) -> X end, Fun(iri(IRI)))
   end.

iri(undefined) ->
   undefined;
iri(IRI) ->
   {ok, Access} = permit:to_access(IRI),
   Access.

%%
scope() ->
   fun(Fun, Scope) ->
      lens:fmap(fun(X) -> X end, Fun(scope(Scope)))
   end.

scope(undefined) ->
   #{};
scope(Scope) ->
   decode(uri:unescape(Scope)).

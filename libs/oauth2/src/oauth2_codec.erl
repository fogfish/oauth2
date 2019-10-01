-module(oauth2_codec).

-include_lib("oauth2/include/oauth2.hrl").
-compile({parse_transform, category}).
-compile({parse_transform, generic}).

-export([
   decode/1
,  authorization/0
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


iri() ->
   fun(Fun, IRI) ->
      lens:fmap(fun(X) -> X end, Fun(iri(IRI)))
   end.

iri(IRI) ->
   [Suffix, Prefix] = binary:split(IRI, <<$@>>),
   {iri, Prefix, Suffix}.

scope() ->
   fun
   (Fun, undefined) ->
      lens:fmap(fun(X) -> X end, Fun(undefined));
   (Fun, Scope) ->
      lens:fmap(fun(X) -> X end, Fun(decode(uri:unescape(Scope))))
   end.
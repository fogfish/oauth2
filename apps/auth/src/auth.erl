-module(auth).

-compile({parse_transform, category}).
-compile({parse_transform, generic}).
-include_lib("oauth2/include/oauth2.hrl").

-export([main/1]).

%%
%%
main(Opts) ->
   {ok, _} = application:ensure_all_started(?MODULE),
   serverless:spawn(fun api/1, Opts).

%%
%%
-spec api(map()) -> datum:either(map()).

api(#{
   <<"httpMethod">> := <<"POST">>
} = Json) ->
   serverless_api:return(dispatch(Json));
api(_) -> 
   {error, not_supported}.

%%
%%
dispatch(#{
   <<"path">> := <<"/signin">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>,
      <<"Authorization">> := Digest
   },
   <<"body">> := Request
}) ->
   [either ||
      oauth2:auth_client_confidential(Digest),
      request(Request),
      cats:unit(labelled_of:authorization(_))
   ];

dispatch(#{
   <<"path">> := <<"/signin">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   },
   <<"body">> := Request
}) ->
   [either ||
      #authorization{
         client_id = Client
      } = Auth <- request(Request),
      oauth2:auth_client_public(Client),
      cats:unit(labelled_of:authorization(Auth))
   ];

dispatch(#{
   <<"path">> := <<"/signup">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>,
      <<"Authorization">> := Digest
   },
   <<"body">> := Request
}) ->
   [either ||
      oauth2:auth_client_confidential(Digest),
      request(Request),
      cats:unit(labelled_of:authorization(_))
   ];

dispatch(#{
   <<"path">> := <<"/signup">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   },
   <<"body">> := Request
}) ->
   [either ||
      #authorization{
         client_id = Client
      } = Auth <- request(Request),
      oauth2:auth_client_public(Client),
      cats:unit(labelled_of:authorization(Auth))
   ];

dispatch(_) ->
   {error, not_supported}.

%%
%%
-spec request(binary()) -> datum:either(#authorization{}).

request(Request) ->
   {ok, [identity ||
      binary:split(Request, <<$&>>, [trim, global]),
      lists:map(fun as_pair/1, _),
      maps:from_list(_),
      labelled_to:authorization(_)
   ]}.

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).

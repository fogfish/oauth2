-module(client).

-compile({parse_transform, category}).
-compile({parse_transform, generic}).

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
   <<"httpMethod">> := <<"GET">>,
   <<"path">> := <<"/oauth2/client">>,
   <<"headers">> := #{
      <<"Authorization">> := <<"Bearer ", Token/binary>>
   }
}) ->
   serverless_api:return(
      oauth2_client:lookup(Token)
   );

api(#{
   <<"httpMethod">> := <<"POST">>,
   <<"path">> := <<"/oauth2/client">>,
   <<"headers">> := #{
      <<"Authorization">> := <<"Bearer ", Token/binary>>,
      <<"content-type">> := <<"application/json", _/binary>>
   },
   <<"body">> := Json
}) ->
   serverless_api:return(
      oauth2_client:create(Token, jsx:decode(Json, [return_maps]))
   );

api(#{
   <<"httpMethod">> := <<"DELETE">>,
   <<"path">> := <<"/oauth2/client/", _/binary>>,
   <<"headers">> := #{
      <<"Authorization">>  := <<"Bearer ", Token/binary>>
   },
   <<"pathParameters">> := #{<<"id">> := Id}
}) ->
   serverless_api:return(
      oauth2_client:remove(Token, Id)
   );

api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

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
      <<"authorization">>  := <<"Bearer ", _/binary>>
   }
}) ->
   serverless_api:return({ok, []});

api(#{
   <<"httpMethod">> := <<"POST">>,
   <<"path">> := <<"/oauth2/client">>,
   <<"headers">> := #{
      <<"authorization">>  := <<"Bearer ", _/binary>>
   }
}) ->
   serverless_api:return({ok, []});

api(#{
   <<"httpMethod">> := <<"DELETE">>,
   <<"path">> := <<"/oauth2/client/", _/binary>>,
   <<"headers">> := #{
      <<"authorization">>  := <<"Bearer ", _/binary>>
   }
}) ->
   serverless_api:return({ok, []});

api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

-module(token).

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
   <<"httpMethod">> := <<"POST">>,
   <<"path">> := <<"/oauth2/token">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   case oauth2:token(Headers, Request) of
      {ok, {uri, _, _} = Redirect} ->
         serverless_api:return({302, #{<<"Location">> => uri:s(Redirect)}, <<>>});
      Other ->
         serverless_api:return(Other)
   end;

api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

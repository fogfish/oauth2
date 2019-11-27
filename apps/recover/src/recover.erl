-module(recover).

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
   <<"path">> := <<"/oauth2/reset">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   case oauth2:reset(Headers, Request) of
      {ok, Redirect} ->
         serverless_api:return({302, #{<<"Location">> => uri:s(Redirect)}, <<>>});
      {error, _} = Error ->
         serverless_api:return(Error)
   end;

api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

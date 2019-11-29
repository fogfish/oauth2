-module(authorize).

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
   <<"httpMethod">> := <<"POST">>
} = Json) ->
   case dispatch(Json) of
      {ok, Redirect} ->
         serverless_api:return({302, #{<<"Location">> => uri:s(Redirect)}, <<>>});
      {error, _} = Error ->
         serverless_api:return(Error)
   end;
api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

%%
%%
dispatch(#{
   <<"path">> := <<"/oauth2/signin">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   oauth2:signin(Headers, Request);

dispatch(#{
   <<"path">> := <<"/oauth2/signup">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   oauth2:signup(Headers, Request);

dispatch(#{
   <<"path">> := <<"/oauth2/reset">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   oauth2:reset(Headers, Request);

dispatch(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

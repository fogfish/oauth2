-module(auth).

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
dispatch(#{<<"path">> := <<"/signin">>}) ->
   {ok, #{<<"ok">> => <<"signin">>}};

dispatch(#{<<"path">> := <<"/signup">>}) ->
   {ok, #{<<"ok">> => <<"signup">>}};

dispatch(_) ->
   {error, not_supported}.

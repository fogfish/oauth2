-module(auth).

-export([main/1]).


%%
%%
main(Opts) ->
   serverless:spawn(fun identity/1, Opts).   


%%
%%
-spec identity(map()) -> datum:either(map()).

identity(Json) ->
   {ok, Json}.

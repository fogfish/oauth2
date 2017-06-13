-module(oauth2).
-compile({parse_transform, category}).
-include("oauth2.hrl").

-export([start/0]).

%%
start() ->
   applib:boot(?MODULE, code:where_is_file("sys.config")).

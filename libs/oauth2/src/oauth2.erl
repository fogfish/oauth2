-module(oauth2).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").

-export([
   signup/2
,  signin/2
,  reset/2
,  token/2
]).

%%
%%
-spec signup(#{}, binary()) -> datum:either(uri:uri()).

signup(Headers, Request) ->
   oauth2_authorize:signup(Headers, Request).

%%
%%
-spec signin(#{}, binary()) -> datum:either(uri:uri()).

signin(Headers, Request) ->
   oauth2_authorize:signin(Headers, Request).

%%
%%
-spec reset(#{}, binary()) -> datum:either(uri:uri()).

reset(Headers, Request) ->
   oauth2_authorize:reset(Headers, Request).

%%
%%
-spec token(#{}, binary()) -> datum:either(#{}).

token(Headers, Request) ->
   oauth2_access:token(Headers, Request).

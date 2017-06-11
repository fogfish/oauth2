-module(oauth2_app).
-behaviour(application).

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   config_root_access(),
   config_ux(),
   oauth2_sup:start_link(). 

%%
%%
stop(_State) ->
   ok.

%%
%%
config_root_access() ->
   permit:create(oauth2ux, crypto:strong_rand_bytes(30), [oauth2ux]).

%%
%%
config_ux() ->
   config_ux(oauth2_signin).

config_ux(Mod) ->
   Root = code:priv_dir(oauth2),
   File = filename:join([Root, scalar:c(Mod) ++ ".html"]),
   {ok, Data} = file:read_file(File),
   {ok, _, Code} = swirl:c(Mod, #{ux => scalar:c(Data)}),
   code:load_binary(Mod, [], Code).   


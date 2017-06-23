%%
%%   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%
-module(oauth2_app).
-behaviour(application).
-compile({parse_transform, category}).

-include("oauth2.hrl").

-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   {ok, Sup} = oauth2_sup:start_link(),
   config_root_access(),
   config_ux(),
   {ok, Sup}.

%%
%%
stop(_State) ->
   ok.


%%
%%
config_root_access() ->
   permit:create(?OAUTH2_UX, crypto:strong_rand_bytes(30),
      #{
         <<"oauth2client">> => <<"public">>,
         <<"redirect_uri">> => ?OAUTH2_UX_CALLBACK
      }
   ).

   % [either ||
   %    permit:create(?OAUTH2_UX, crypto:strong_rand_bytes(30), [oauth2client, public]),
   %    pts:put(oauth2client, ?OAUTH2_UX, 
   %       #{
   %          <<"type">>         => <<"public">>,
   %          <<"redirect_uri">> => ?OAUTH2_UX_CALLBACK
   %       }
   %    )
   % ].

%%
%%
config_ux() ->
   config_ux(oauth2_signin),
   config_ux(oauth2_signup).

config_ux(Mod) ->
   Root = code:priv_dir(oauth2),
   File = filename:join([Root, scalar:c(Mod) ++ ".html"]),
   {ok, Data} = file:read_file(File),
   {ok, _, Code} = swirl:c(Mod, #{ux => scalar:c(Data)}),
   code:load_binary(Mod, [], Code).   


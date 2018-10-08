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
   permit:config(),
   {ok, Sup} = oauth2_sup:start_link(),
   config_root_access(),
   {ok, Sup}.

%%
%%
stop(_State) ->
   ok.


%%
%%
config_root_access() ->
   oauth2_client:create("oauth2-account", crypto:strong_rand_bytes(30),
      #{
         <<"type">>         => <<"oauth2:client">>,
         <<"security">>     => <<"public">>,
         <<"redirect_uri">> => uri:s(uri:path("/oauth2/account", uri:new(opts:val(issuer, permit))))
      }
   ).


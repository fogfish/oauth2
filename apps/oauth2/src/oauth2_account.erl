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
%%   account management 
-module(oauth2_account).
-compile({parse_transform, category}).

-export([
   profile/1
]).


%%
%%
-spec profile(permit:access()) -> {ok, map()} | {error, _}.

profile(Access) ->
   [either ||
      permit:lookup(Access),
      fmap(maps:with([<<"access">>, <<"roles">>], _)),
      lookup_clients(_)
   ].

lookup_clients(#{<<"access">> := Access} = Profile) ->
   [either ||
      pts:call(permit, Access, {match, <<"master">>, [{<<"master">>, Access}]}),
      lookup_clients_info(_),
      fmap(Profile#{<<"clients">> => _})
   ].

lookup_clients_info(List) ->
   {ok, lists:map(
      fun(#{<<"access">> := Access} = Basic) ->
         {ok, Client} = oauth2_client:lookup(Access),
         maps:merge(Basic, Client)
      end,
      List
   )}.

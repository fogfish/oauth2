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
%%   build an oauth2 user experience
-module(oauth2_ux).
-compile({parse_transform, category}).

-export([
   signin/1,
   signup/1
]).

%%
%% render sign-in user experience
-spec signin([_]) -> {ok, binary()} | {error, _}.

signin(Env) ->
   build_ux(oauth2_signin, Env).

%%
%% render sign-up user experience
-spec signup([_]) -> {ok, binary()} | {error, _}.

signup(Env) ->
   build_ux(oauth2_signup, Env).

%%
%%
build_ux(Mod, Env) ->
   [either ||
      oauth2_req:issue_access_code(Env),
      fmap(build_ux_error(_)),
      build_ux_spec(_),
      Mod:ux(_)
   ].

build_ux_spec(Env) ->
   %% @todo: protect template from injections
   {ok, #{ux => [{scalar:atom(Key), Val} || {Key, Val} <- maps:to_list(Env)]}}.

build_ux_error(Env) ->
   %% @todo: move errors localization into template
   Lens = lens:map(<<"error">>, undefined),
   case lens:get(Lens, Env) of
      undefined ->
         Env;
      <<"unauthorized">> ->
         lens:put(Lens, <<"Invalid credentials.">>, Env);
      _ ->
         lens:put(Lens, <<"System error, try later.">>, Env)
   end.

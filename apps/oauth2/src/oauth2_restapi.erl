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
%%   common restapi utility
-module(oauth2_restapi).
-compile({parse_transform, category}).
-include("oauth2.hrl").

-export([
   decode/1,
   authenticate/2,
   access_token/1
]).



%%
%% decodes oauth2 request
%% parse application/x-www-form-urlencoded to map
-spec decode(_) -> {ok, _} | {error, _}.  

decode(Request) ->
   {ok, [$. ||
      binary:split(scalar:s(Request), <<$&>>, [trim, global]),
      lists:map(fun as_pair/1, _),
      maps:from_list(_)
   ]}.

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).


%%
%% authenticate client and injects its identity into environment
-spec authenticate(_, _) -> ok | {error, _}.

authenticate(#{<<"client_id">> := Access} = Env, Head) ->
   [either ||
      oauth2_client:lookup(Access),
      authenticate_client(_, Env, Head)
   ];

authenticate(Env, Head) ->
   [either ||
      category:optionT(unauthorized_client,
         lens:get( lens:pair('Authorization', undefined), Head )
      ),
      authenticate_http_digest(_, Env)
   ].

%%
authenticate_client(#{<<"security">> := <<"public">>}, Env, _) ->
   {ok, Env};
authenticate_client(#{<<"security">> := <<"confidential">>}, Env, Head) ->
   [either ||
      category:optionT(unauthorized_client,
         lens:get( lens:pair('Authorization', undefined), Head )
      ),
      authenticate_http_digest(_, Env)
   ].

%%
authenticate_http_digest(<<"Basic ", Digest/binary>>, Env) ->
   [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
   [either ||
      permit:stateless(Access, Secret, 1, #{}),
      oauth2_client:lookup(Access),
      oauth2_client:is_confidential(_),
      fmap(Env#{<<"client_id">> => Access, <<"client_secret">> => Secret})
   ];
authenticate_http_digest(_, _) ->
   {error, unauthorized_client}.


%%
%%
access_token(Head) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         Token;
      _ ->
         undefined
   end.


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

-export([
   decode/1,
   access_token/1,
   validate_client_type/1,
   validate_redirect_uri/1
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
%%
-spec access_token(_) -> _ | undefined.

access_token(Head) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         Token;
      _ ->
         undefined
   end.


%%
%% 2.1. Client Types
-spec validate_client_type(map()) -> {ok, map()} | {error, _}.

validate_client_type(#{<<"type">> := <<"public">>} = Req) ->
   {ok, Req};
validate_client_type(#{<<"type">> := <<"confidential">>} = Req) ->
   {ok, Req};
validate_client_type(_) ->
   {error, invalid_type}.

%%
%%
-spec validate_redirect_uri(map()) -> {ok, map()} | {error, _}.

validate_redirect_uri(#{<<"redirect_uri">> := Uri} = Req) ->
   [either ||
      fmap( uri:new(Uri) ),
      assert_some(fun uri:schema/1, _),
      assert_some(fun uri:authority/1, _),
      assert_some(fun uri:path/1, _),
      assert_none(fun uri:q/1, _),
      assert_none(fun uri:anchor/1, _),
      fmap(Req)
   ].


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------


assert_some(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {error, invalid_uri};
      _ ->
         {ok, Uri}
   end.

assert_none(Fun, Uri) ->
   case Fun(Uri) of
      undefined ->
         {ok, Uri};
      _ ->
         {error, invalid_uri}
   end.

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
%%   introspection endpoint
%%   https://tools.ietf.org/html/rfc7662
-module(oauth2_restapi_introspect).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
%% 2.1. Introspection Request
%%   To prevent token scanning attacks, the endpoint MUST also require
%%   some form of authorization to access this endpoint
authorize(_Mthd, {_Uri, Head, _Env}) ->
   case permit_oauth2:authenticate(Head) of
      {error, undefined} ->
         {error, unauthorized};
      {ok, _Access} ->
         ok;
      {error, _} = Error ->
         Error
   end.

%%
'POST'(_Type, Req, {_Uri, _Head, _Env}) ->
   [either ||
      fmap(permit_oauth2:decode(Req)),
      fmap(lens:get(lens:pair(<<"token">>), _)),
      permit:validate(_),
      fmap(jsx:encode(_))
   ].   

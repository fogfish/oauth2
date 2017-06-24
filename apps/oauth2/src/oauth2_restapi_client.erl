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
-module(oauth2_restapi_client).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'GET'/3,
   'DELETE'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST', 'DELETE'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
     % ,category:maybeT(unauthorized, lens:get(lens:map(<<"oauth2developer">>, undefined)))
   ].

%%
%% 
'GET'(_Type, _Req, {_Uri, _Head, Env}) ->
   [either ||
      category:maybeT(badarg, lens:get(lens:pair(<<"id">>), Env))
     ,permit:lookup(_)
     ,permit_pubkey:claims(_)
     ,fmap(jsx:encode(_))
   ].

%%
%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      oauth2_restapi:decode(Req),
      oauth2_kvs_client:validate(_),
      create_profile(oauth2_restapi:access_token(Head), _)
   ].

create_profile(Token, Profile) ->
   case permit:pubkey(Token, [oauth2client]) of
      {ok, #{<<"access">> := Access} = PubKey} ->
         [either ||
            oauth2_kvs_client:create(Access, Profile),
            fmap(jsx:encode(PubKey))
         ];
      {error,   _} = Error ->
         Error
   end.


%%
%%
'DELETE'(_Type, _Req, {_Uri, _Head, Env}) ->
   %% find user by id from token
   %% validate that user owns resource
   %% delete the resource
   [either ||
      category:maybeT(badarg, uri:unescape(lens:get(lens:pair(<<"id">>), Env)))
     % ,oauth2_client:remove(oauth2_restapi:access_token(Head), _)
     ,oauth2_client:remove(_)
     ,fmap(jsx:encode(_))
   ].

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
-module(oauth2_restapi_token).
-compile({parse_transform, category}).

-include("oauth2.hrl").

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
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
%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      oauth2_restapi:decode(Req),
      oauth2_restapi:authenticate(_, Head),
      oauth2_issue_access_token(_),
      fmap(jsx:encode(_))
   ].

%%
%%
oauth2_issue_access_token(#{<<"grant_type">> := <<"authorization_code">>, <<"code">> := Token}) ->
   [either ||
      permit:include(Token, ?OAUTH2_EXCH),
      access_token(Token)
   ];

oauth2_issue_access_token(#{<<"grant_type">> := <<"password">>, <<"username">> := Access, <<"password">> := Secret}) ->
   [either ||
      permit:stateless(Access, Secret, ?OAUTH2_TTL_CODE, permit:default_claims()),
      access_token(_)
   ];   

oauth2_issue_access_token(#{<<"grant_type">> := <<"client_credentials">>, <<"client_id">> := Access, <<"client_secret">> := Secret}) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:is_confidential(_),
      permit:stateless(Access, Secret, ?OAUTH2_TTL_CODE, permit:default_claims()),
      access_token(_)
   ];

oauth2_issue_access_token(#{<<"grant_type">> := <<"refresh_token">>, <<"refresh_token">> := Token}) ->
   [either ||
      permit:include(Token, ?OAUTH2_EXCH),
      access_token(Token)
   ];

oauth2_issue_access_token(_) ->
   {error, invalid_request}.

%%
%%
access_token(Token) ->
   [either ||
      create_empty_token(),
      create_access_token(Token, _),
      create_refresh_token(Token, _)
   ].

create_empty_token() ->
   {ok, 
      #{<<"token_type">> => <<"bearer">>, <<"expires_in">> => ?OAUTH2_TTL_ACCESS}
   }.

create_access_token(Token, X) ->
   [either ||
      permit:stateless(Token, ?OAUTH2_TTL_ACCESS, permit:default_claims()),
      fmap(X#{<<"access_token">> => _})
   ].

create_refresh_token(Token, X) ->
   [either ||
      permit:revocable(Token, ?OAUTH2_TTL_REFRESH, ?OAUTH2_EXCH),
      fmap(X#{<<"refresh_token">> => _})
   ].

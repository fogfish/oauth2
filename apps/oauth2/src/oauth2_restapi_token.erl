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
oauth2_issue_access_token(#{<<"grant_type">> := <<"authorization_code">>, <<"code">> := Code}) ->
   [either ||
      permit:validate(Code),
      category:maybeT(server_error,
         lens:get(lens:map(<<"sub">>), _)
      ),
      access_token(_)
   ];

oauth2_issue_access_token(#{<<"grant_type">> := <<"password">>, <<"username">> := Access, <<"password">> := Secret}) ->
   [either ||
      permit:auth(Access, Secret, 3600),
      category:maybeT(server_error,
         lens:get(lens:map(<<"sub">>), _)
      ),
      access_token(_)
   ];   

oauth2_issue_access_token(#{<<"grant_type">> := <<"client_credentials">>, <<"client_id">> := Access}) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:is_confidential(_),
      access_token(Access)
   ];

% oauth2_issue_access_token(#{<<"grant_type">> := <<"refresh_token">>} = Env) ->

oauth2_issue_access_token(_) ->
   {error, invalid_request}.

%%
%%
access_token(Access) ->
   [either ||
      permit:issue(Access, 3600),
      fmap(lens:put(lens:map(<<"access_token">>, undefined), _, #{})),
      fmap(lens:put(lens:map(<<"token_type">>, undefined), <<"bearer">>, _)),
      fmap(lens:put(lens:map(<<"expires_in">>, undefined), 3600, _))
   ].



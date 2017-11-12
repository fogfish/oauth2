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
%% 
'GET'(_Type, _Req, {_Uri, Head, Env}) ->
   [either ||
      category:optionT(unauthorized, oauth2_restapi:access_token(Head)),
      permit:validate(_),
      lookup(_, Env)
   ].

lookup(#{<<"sub">> := Master}, Env) ->
   [either ||
      category:optionT(badarg, 
         lens:get(lens:pair(<<"id">>), Env)
      ),
      oauth2_client:lookup(_),
      oauth2_client:is_master(_, Master),
      unit(jsx:encode(_))
   ].

%%
%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      category:optionT(unauthorized, oauth2_restapi:access_token(Head)),
      permit:validate(_),
      create(_, Req)
   ].

create(#{<<"sub">> := Master}, Req) ->
   [either ||
      oauth2_restapi:decode(Req),
      oauth2_client:create(Master, _),
      unit(jsx:encode(_))
   ].

%%
%%
'DELETE'(_Type, _Req, {_Uri, Head, Env}) ->
   [either ||
      category:optionT(unauthorized, oauth2_restapi:access_token(Head)),
      permit:validate(_),
      remove(_, Env)
   ].

remove(#{<<"sub">> := Master}, Env) ->
   [either ||
      category:optionT(badarg, 
         lens:get(lens:pair(<<"id">>), Env)
      ),
      oauth2_client:lookup(_),
      oauth2_client:is_master(_, Master),

      category:optionT(badarg, 
         lens:get(lens:pair(<<"id">>), Env)
      ),
      oauth2_client:remove(_),
      unit(jsx:encode(_))
   ].


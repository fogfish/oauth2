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
-module(oauth2_restapi_account).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   authorize/2,
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
   ].

'GET'(_Type, _Req, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
     ,fmap(lens:get(lens:map(<<"sub">>), _))
     ,oauth2_account:profile(_)
     ,jsx:encode(_)
   ].

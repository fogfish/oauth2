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
-module(oauth2_restapi_auth).
-include("oauth2.hrl").
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST'].

%%
content_provided(_Req) ->
   [{text, html}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
%%
'GET'(_Type, _, {Uri, _Head, _Env}) ->
   case uri:q(Uri) of
      undefined ->
         oauth2_ux:signin(#{<<"response_type">> => <<"code">>, <<"client_id">> => ?OAUTH2_UX});
      Query ->
         oauth2_ux:signin(maps:from_list(Query))
   end.

%%
%%
'POST'(_Type, Req, {Uri, Head, Env}) ->
   case 
      [either ||
         fmap(permit_oauth2:decode(Req)),
         oauth2_req:auth_access_code(_),
         oauth2_req:auth_client_id(_),
         oauth2_req:auth_pubkey(_),
         oauth2_req:redirect_uri(_)
      ]
   of
      %% @todo: make error by standard
      {ok, Location}   ->
         {302, [{'Location', Location}], <<>>};

      {error, expired} ->
         UriWithError = uri:q(permit_oauth2:decode(Req), Uri),
         'GET'(undefined, undefined, {UriWithError, Head, Env});

      {error, _} = Error  ->
         UriWithError = uri:q([Error | permit_oauth2:decode(Req)], Uri),
         'GET'(undefined, undefined, {UriWithError, Head, Env})
   end.

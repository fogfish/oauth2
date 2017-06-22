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
-compile({parse_transform, category}).
-include("oauth2.hrl").


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
'GET'(_Type, Req, {Uri, Head, _Env}) ->
   [either ||
      oauth2_req:new(Uri, Head, Req),
      oauth2_req:authenticate(_),
      oauth2_ux:signin(_)
   ].


   % case uri:suburi(Uri) of
   %    ?OAUTH2_UX_ROOT ->
   %       {302, [{'Location', ?OAUTH2_UX_URL}], <<>>};
   %    _ ->
   %       oauth2_ux:signin(maps:from_list(uri:q(Uri)))
   % end.

%%
%%
'POST'(_Type, Req, {Uri, Head, _Env}) ->
   case
      [either ||
         oauth2_req:new(Uri, Head, Req),
         oauth2_req:authenticate(_),
         oauth2_req:check_access_code(_),
         oauth2_req:check_pubkey(_),
         oauth2_req:redirect_to(_)
      ]
   of
      {ok,  Location} ->
         {302, [{'Location', Location}], <<>>};

      {error, Reason} ->
         io:format("==> ~p ~p~n", [Reason, oauth2_req:new(Uri, Head, Req)]),
         {ok, Error} = [either ||
            oauth2_req:new(Uri, Head, Req),
            oauth2_req:authenticate(_),
            oauth2_req:redirect_to(Reason, _)
         ],
         {302, [{'Location', Error}], <<>>}
   end.

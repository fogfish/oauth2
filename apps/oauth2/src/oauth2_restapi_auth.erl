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
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
%%
'GET'(_Type, _Req, {_Uri, _Head, _Env}) ->
   {ok, <<>>}.

%%
%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   case
      [either ||
         oauth2_restapi:decode(Req),
         oauth2_restapi:auth_client(_, Head),
         oauth2_grant_flow(_)
      ]
   of
      {ok,  Uri} ->
         {302, [{'Location', Uri}], <<>>};

      {error, _} = Error ->
         Error
   end.

%%
%%
oauth2_grant_flow(#{<<"response_type">> := <<"code">>} = Env) ->
   case oauth2_code_grant_flow(Env) of
      {error,  Reason} ->
         oauth2_redirect_with_error(Reason, Env);
      {ok, _} = Result ->
         Result
   end;

oauth2_grant_flow(#{<<"response_type">> := <<"token">>} = Env) ->
   case oauth2_implicit_grant_flow(Env) of
      {error,  Reason} ->
         oauth2_redirect_with_error(Reason, Env);
      {ok, _} = Result ->
         Result
   end;

oauth2_grant_flow(Env) ->
   oauth2_redirect_with_error(unsupported_response_type, Env).

%%
%%
oauth2_code_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret} = Env) ->
   [either ||
      permit:auth(Access, Secret, 600),
      oauth2_redirect_code_grant(_, Env)
   ].

oauth2_redirect_code_grant(Token, #{<<"client_id">> := Access} = Env) ->
   [either ||
      permit:lookup(Access),
      redirect_uri(<<"confidential">>, _),
      oauth2_redirect_to(
         _,
         [
            {code, Token}, 
            {state, lens:get(lens:map(<<"state">>, undefined), Env)}
         ]
      )
   ].

%%
%%
oauth2_implicit_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret} = Env) ->
   [either ||
      permit:auth(Access, Secret, 3600),
      oauth2_redirect_implicit_grant(_, Env)
   ].

oauth2_redirect_implicit_grant(Token, #{<<"client_id">> := Access} = Env) ->
   [either ||
      permit:lookup(Access),
      redirect_uri(<<"public">>, _),
      oauth2_redirect_to(
         _,
         [
            {access_token, Token}, 
            {state, lens:get(lens:map(<<"state">>, undefined), Env)}
         ]
      )
   ].


%%
%%
oauth2_redirect_with_error(Reason, #{<<"client_id">> := Access} = Env) ->
   [either ||
      permit:lookup(Access),
      category:maybeT(server_error,
         lens:get( lens:map(<<"redirect_uri">>, undefined), Env )
      ),
      oauth2_redirect_to(
         _,
         [
            {error, Reason}, 
            {state, lens:get(lens:map(<<"state">>, undefined), Env)}
         ]
      )
   ].

%%
%%
oauth2_redirect_to(Uri, Query) ->
   {ok, [$. ||
      uri:new(Uri),
      uri:q(Query, _),
      uri:s(_)
   ]}.

%%
%%
redirect_uri(Type, #{<<"oauth2client">> := Type, <<"redirect_uri">> := Uri}) ->
   {ok, Uri};
redirect_uri(_, _) ->
   {error, invalid_request}.


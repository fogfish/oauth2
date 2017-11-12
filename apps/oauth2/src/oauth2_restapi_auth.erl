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
%%   RFC 6749: 
%%    To request an access token, the client obtains authorization from the
%%    resource owner.  The authorization is expressed in the form of an
%%    authorization grant, which the client uses to request the access
%%    token.
%%
%%   Endpoint implements Authorization request as defined by
%%   https://tools.ietf.org/html/rfc6749
%%    Section 4.1.1.  Authorization Request 
%%    Section 4.2.1.  Authorization Request
%%
-module(oauth2_restapi_auth).
-compile({parse_transform, category}).
-include("oauth2.hrl").


%%
%%
oauth2_grant_flow(#{<<"response_type">> := <<"code">>, <<"client_id">> := Access} = Request) ->
   case
      [either ||
         oauth2_client:lookup(Access),
         oauth2_code_grant_flow(Request)
      ]
   of
      {error,  Reason} ->
         oauth2_redirect_with_error(Reason, Request);
      {ok, _} = Result ->
         Result
   end;

oauth2_grant_flow(#{<<"response_type">> := <<"token">>, <<"client_id">> := Access} = Request) ->
   case
      [either ||
         oauth2_client:lookup(Access),
         oauth2_implicit_grant_flow(Request)
      ]
   of
      {error,  Reason} ->
         oauth2_redirect_with_error(Reason, Request);
      {ok, _} = Result ->
         Result
   end;

oauth2_grant_flow(Request) ->
   oauth2_redirect_with_error(unsupported_response_type, Request).


% %%
% %%
% 'POST'(_Type, Req, {_Uri, Head, _Env}) ->
%    case
%       [either ||
%          oauth2_restapi:decode(Req),
%          oauth2_restapi:authenticate(_, Head),
%          oauth2_grant_flow(_)
%       ]
%    of
%       {ok,  Uri} ->
%          {302, [{'Location', Uri}], <<>>};

%       {error, _} = Error ->
%          Error
%    end.


%%
%%
oauth2_code_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret, <<"oauth2">> := <<"signup">>} = Env) ->
   [either ||
      oauth2_account:create(Access, Secret,
         #{
            <<"type">> => <<"oauth2:account">>,
            <<"uid">>  => true
         }
      ),
      permit:stateless(Access, Secret, ?OAUTH2_TTL_CODE, ?OAUTH2_EXCH),
      oauth2_code_grant_redirect(_, Env)
   ];

oauth2_code_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret} = Env) ->
   [either ||
      permit:stateless(Access, Secret, ?OAUTH2_TTL_CODE, ?OAUTH2_EXCH),
      oauth2_code_grant_redirect(_, Env)
   ].

oauth2_code_grant_redirect(Token, #{<<"client_id">> := Access} = Env) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:redirect_uri(_,
         [
            {code, Token}, 
            {state, lens:get(lens:map(<<"state">>, undefined), Env)}
         ]
      )
   ].

%%
%%
oauth2_implicit_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret, <<"oauth2">> := <<"signup">>} = Env) ->
   [either ||
      oauth2_account:create(Access, Secret,
         #{
            <<"type">> => <<"oauth2:account">>,
            <<"uid">>  => true
         }
      ),
      permit:stateless(Access, Secret, ?OAUTH2_TTL_ACCESS, permit:default_claims()),
      oauth2_implicit_grant_redirect(_, Env)      
   ];

oauth2_implicit_grant_flow(#{<<"access">> := Access, <<"secret">> := Secret} = Env) ->
   [either ||
      permit:stateless(Access, Secret, ?OAUTH2_TTL_ACCESS, permit:default_claims()),
      oauth2_implicit_grant_redirect(_, Env)
   ].

oauth2_implicit_grant_redirect(Token, #{<<"client_id">> := Access} = Env) ->
   [either ||
      oauth2_client:lookup(Access),
      oauth2_client:redirect_uri(_,
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
      oauth2_client:lookup(Access),
      oauth2_client:redirect_uri(_, 
         [
            {error, Reason}, 
            {state, lens:get(lens:map(<<"state">>, undefined), Env)}
         ]
      )
   ].

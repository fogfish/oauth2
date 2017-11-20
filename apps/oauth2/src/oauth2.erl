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
-module(oauth2).
-compile({parse_transform, category}).
-include("oauth2.hrl").
-include_lib("common_test/include/ct.hrl").


-export([start/0]).
-export([
   signin/2,
   signup/2,
   token/2
]).

%%
start() ->
   applib:boot(?MODULE, code:where_is_file("sys.config")).


%%
%%
signin(#{<<"response_type">> := <<"code">>, <<"access">> := Access, <<"secret">> := Secret} = Request, Client) ->
   case 
      oauth2_token:exchange_code(Access, Secret) 
   of
      {ok, Code} ->
         redirect_uri({code, Code}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signin(#{<<"response_type">> := <<"token">>, <<"access">> := Access, <<"secret">> := Secret} = Request, Client) ->
   case 
      oauth2_token:access(Access, Secret) 
   of
      {ok, Token} ->
         redirect_uri({access_token, Token}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signin(Request, Client) ->
   redirect_uri({error, unsupported_response_type}, Request, Client).


%%
%%
signup(#{<<"response_type">> := <<"code">>, <<"access">> := Access, <<"secret">> := Secret} = Request, Client) ->
   case 
      [either ||
         create_account(Request),
         oauth2_token:exchange_code(Access, Secret)
      ]
   of
      {ok, Code} ->
         redirect_uri({code, Code}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signup(#{<<"response_type">> := <<"token">>, <<"access">> := Access, <<"secret">> := Secret} = Request, Client) ->
   case 
      [either ||
         create_account(Request),
         oauth2_token:access(Access, Secret)
      ]
   of
      {ok, Token} ->
         redirect_uri({access_token, Token}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signup(Request, Client) ->
   redirect_uri({error, unsupported_response_type}, Request, Client).


%%
%%
token(#{<<"grant_type">> := <<"authorization_code">>, <<"code">> := Code}, _Client) ->
   [either ||
      oauth2_token:is_exchangable(Code),
      oauth2_token:bearer(Code)
   ];

token(#{<<"grant_type">> := <<"password">>, <<"username">> := Access, <<"password">> := Secret}, _Client) ->
   [either ||
      oauth2_token:access(Access, Secret),
      oauth2_token:bearer(_)
   ];

token(#{<<"grant_type">> := <<"client_credentials">>}, #{<<"identity">> := Token} = Client) ->
   [either ||
      oauth2_client:is_confidential(Client),
      oauth2_token:bearer(Token)
   ];

token(#{<<"grant_type">> := <<"refresh_token">>, <<"refresh_token">> := Token}, Client) ->
   [either ||
      oauth2_token:is_exchangable(Token),
      oauth2_client:is_confidential(Client),
      oauth2_token:bearer(Token)
   ];

token(_, _) ->
   {error, invalid_request}.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
create_account(#{<<"access">> := Access, <<"secret">> := Secret}) ->
   oauth2_account:create(Access, Secret,
      #{
         <<"type">> => <<"oauth2:account">>,
         <<"uid">>  => true
      }
   ).


%%
redirect_uri(Status, Request, Client) ->
   State = lens:get(lens:at(<<"state">>, undefined), Request),
   oauth2_client:redirect_uri(Client, [Status, {state, State}]).


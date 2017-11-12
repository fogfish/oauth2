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


-export([start/0]).
-export([
   signin/2,
   signup/2
]).

%%
start() ->
   applib:boot(?MODULE, code:where_is_file("sys.config")).


%%
%%
access_token(Access, Secret) ->
   permit:revocable(
      Access, 
      Secret, 
      opts:val(ttl_access_token, 1200, oauth2),
      permit:default_claims()
   ).

access_token(#{<<"access">> := Access, <<"secret">> := Secret}) ->
   access_token(Access, Secret).

%%
%%
exchange_code(Access, Secret) ->
   permit:stateless(
      Access,
      Secret,
      opts:val(ttl_exchange_code, 60, oauth2),
      #{<<"exch">> => true}
   ).

exchange_code(#{<<"access">> := Access, <<"secret">> := Secret}) ->
   exchange_code(Access, Secret).


%%
%%
signin(#{<<"response_type">> := <<"code">>} = Request, Client) ->
   case 
      exchange_code(Request) 
   of
      {ok, Code} ->
         redirect_uri({code, Code}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signin(#{<<"response_type">> := <<"token">>} = Request, Client) ->
   case 
      access_token(Request) 
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
signup(#{<<"response_type">> := <<"code">>} = Request, Client) ->
   case 
      [either ||
         create_account(Request),
         exchange_code(Request) 
      ]
   of
      {ok, Code} ->
         redirect_uri({code, Code}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signup(#{<<"response_type">> := <<"token">>} = Request, Client) ->
   case 
      [either ||
         create_account(Request),
         access_token(Request)
      ]
   of
      {ok, Token} ->
         redirect_uri({access_token, Token}, Request, Client);
      {error, _} = Error ->
         redirect_uri(Error, Request, Client)
   end;

signup(Request, Client) ->
   redirect_uri({error, unsupported_response_type}, Request, Client).


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



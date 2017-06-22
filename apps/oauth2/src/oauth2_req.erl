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
%%   oauth2 req 
-module(oauth2_req).
-include("oauth2.hrl").
-compile({parse_transform, category}).

-export([
   new/3,
   authenticate/1,
   issue_access_code/1,
   check_access_code/1,
   issue_pubkey/1,
   check_pubkey/1,
   redirect_to/1,
   redirect_to/2

   % auth_access_code/1,
   % issue_access_code/1,
   % auth_client/1,
   % auth_pubkey/1,
   % issue_pubkey/1,
   % redirect_uri/1
]).

%%
%% data type(s)
-type req() :: #{

   %% Value MUST be set to "code" | "token"
   response_type  => binary()

   %% The client identifier as described in Section 2.2.
   %% The client secret is defined by client authentication in Section 2.3 
  ,client_id      => binary()
  ,client_secret  => binary()

   %% the client profile
  ,client_type    => binary()
  ,redirect_uri   => binary()

   %%
   %% credentials
  ,access         => binary()
  ,secret         => binary()

   %% Bearer Token
  ,access_token   => binary()

   %% one-time-use access code
  ,access_code    => binary()

   %% An opaque value used by the client to maintain
   %% state between the request and callback.
  ,state          => binary()
}.


%%
%% build a new request from uri, headers and payload
-spec new(_, _, _) -> {ok, req()} | {error, _}.

new(Uri, Head, Req) ->
   A = maps:with(valid_input_http(), http(Head)),
   B = maps:with(valid_input_req(),  decode(scalar:s(Req))),
   C = maps:with(valid_input_uri(),  decode(uri:q(Uri))),
   {ok, maps:merge(A, maps:merge(B, C))}.

http(Head) ->
   case lens:get(lens:pair('Authorization', undefined), Head) of
      <<"Bearer ", Token/binary>> ->
         #{access_token => Token};

      <<"Basic ", Digest/binary>> ->
         [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
         #{client_id => Access, client_secret => Secret};

      _ ->
         #{}
   end.

valid_input_uri() ->
   [response_type, client_id, state].

valid_input_req() ->
   [access, secret, access_code, response_type, client_id, state].

valid_input_http() ->
   [access_token, client_id, client_secret].


%%
%% 2.3. Client Authentication
%%
%% If the client type is confidential, the client and authorization
%% server establish a client authentication method...
%% 
%% The authorization server MAY establish a client authentication method
%% with public clients.
-spec authenticate(req()) -> {ok, req()} | {error, _}.

authenticate(#{client_id := Access, client_secret := Secret} = Request) ->
   [either ||
      permit:auth(Access, Secret, 3600, [oauth2client]),
      oauth2_kvs_client:lookup(Access),
      check_client_type(<<"confidential">>, _),
      fmap(
         Request#{
            client_type  => <<"confidential">>,
            redirect_uri => uri:new(lens:get(lens:map(<<"redirect_uri">>), _))
         }
      )
   ];

authenticate(#{client_id := Access} = Request) ->
   [either ||
      oauth2_kvs_client:lookup(Access),
      check_client_type(<<"public">>, _),
      fmap(
         Request#{
            client_type  => <<"public">>,
            redirect_uri => uri:new(lens:get(lens:map(<<"redirect_uri">>), _))
         }
      )
   ];

authenticate(_) ->
   {error, unauthorized}.
   
%%
check_client_type(Type, #{<<"type">> := Type} = Profile) ->
   {ok, Profile};
check_client_type(_, _) ->
   {error, unauthorized}.


%%
%% access code is one-time-use token to prevent replay
-spec issue_access_code(_) -> {ok, _} | {error, _}.

issue_access_code(Request) ->
   [either ||
      permit:issue(?OAUTH2_UX, 300),
      fmap(Request#{access_code => _})
   ].

%%
%%
-spec check_access_code(_) -> {ok, _} | {error, _}.

check_access_code(#{access_code := Code} = Request) ->
   [either ||
      permit:validate(Code),
      fmap(lens:get(lens:map(?OAUTH2_UX), _)),
      fmap(Request)
   ];
check_access_code(_) ->
   {error, unauthorized}.



%%
%%
-spec issue_pubkey(_) -> {ok, _} | {error, _}.

issue_pubkey(_) ->
   ok.

%%
%% authenticate access/secret key
-spec check_pubkey(_) -> {ok, _} | {error, _}.

check_pubkey(#{response_type := <<"code">>, access := Access, secret := Secret} = Request) ->
   [either ||
      permit:auth(Access, Secret, 600),
      fmap(Request#{access_code => _})
   ];

check_pubkey(#{response_type := <<"token">>, access := Access, secret := Secret} = Request) ->
   [either ||
      permit:auth(Access, Secret, 3600),
      fmap(Request#{access_token => _})
   ];

check_pubkey(_) ->
   {error, unsupported_response_type}.



%%
%%
-spec redirect_to(_) -> {ok, _} | {error, _}. 

redirect_to(#{
   client_type  := <<"confidential">>, response_type := <<"code">>,
   redirect_uri := Uri, access_code := Code, state := State}) ->

   {ok, uri:s( uri:q([{code, Code}, {state, State}], Uri) ) };

redirect_to(#{
   client_type  := <<"public">>, response_type := <<"token">>,
   redirect_uri := Uri, access_token := Token, state := State}) ->

   {ok, uri:s( uri:q([{access_token, Token}, {state, State}], Uri) ) }.

redirect_to(Reason, #{redirect_uri := Uri, state := State}) ->
   {ok, uri:s( uri:q([{error, Reason}, {state, State}], Uri) ) }.





% %%
% %% authenticate one-time-use access code to prevent replay.
% -spec auth_access_code(_) -> {ok, _} | {error, _}.

% auth_access_code(#{<<"access_code">> := Code} = Request) ->
%    [either ||
%       permit:validate(Code),
%       fmap(lens:get(lens:map(?OAUTH2_UX), _)),
%       fmap(Request)
%    ].

% %%
% %%
% -spec issue_access_code(_) -> {ok, _} | {error, _}.

% issue_access_code(Request) ->
%    [either ||
%       permit:issue(?OAUTH2_UX, 300),
%       fmap(Request#{<<"access_code">> => _})
%    ].

% %%
% %% authenticate client id and its registry profile
% -spec auth_client(_) -> {ok, _} | {error, _}.

% auth_client(#{<<"client_id">> := AppId} = Request) ->
%    [either ||
%       %% @todo: read registry
%       permit:issue(AppId, 0),
%       fmap(Request#{<<"redirect_uri">> => ?OAUTH2_UX_CALLBACK})
%    ].

% %%
% %% authenticate access/secret key 
% -spec auth_pubkey(_) -> {ok, _} | {error, _}. 

% auth_pubkey(#{<<"access">> := Access, <<"secret">> := Secret} = Request) ->
%    [either ||
%       permit:auth(Access, Secret, 600),
%       fmap(Request#{<<"access_code">> => _})
%    ].

% %%
% %% issue new pubkey based on defined credentials
% -spec issue_pubkey(_) -> {ok, _} | {error, _}. 

% issue_pubkey(#{<<"access">> := Access, <<"secret">> := Secret} = Request) ->
%    [either ||
%       %% @todo: scopes
%       permit:create(Access, Secret),
%       fmap(Request#{<<"access_code">> => _})
%    ].

% %%
% %%
% -spec redirect_uri(_) -> {ok, _} | {error, _}. 

% redirect_uri(#{
%    <<"response_type">> := <<"code">>,
%    <<"redirect_uri">> := Uri, 
%    <<"access_code">> := Code, 
%    <<"state">> := State}) ->

%    Query = [{<<"code">>, Code}, {<<"state">>, State}],
%    {ok, uri:s(uri:q(Query, uri:new(Uri)))}.


% %%
% %% decodes oauth2 request
% %% parse application/x-www-form-urlencoded to map
% -spec decode(_) -> {ok, _} | {error, _}.  

% decode(Request) ->
%    {ok, [$. ||
%       binary:split(scalar:s(Request), <<$&>>, [trim, global]),
%       lists:map(fun as_pair/1, _),
%       maps:from_list(_)
%    ]}.

% as_pair(Pair) ->
%    erlang:list_to_tuple(
%       [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
%    ).







%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% build list of pairs from map
decode(undefined) ->
   decode([]);

decode(X)
 when is_binary(X) ->
   %% parse application/x-www-form-urlencoded to map
   decode( binary:split(X, <<$&>>, [trim, global]) );

decode(X)
 when is_list(X) ->
   maps:from_list( lists:map(fun as_pair/1, X) ).

as_pair({Key, Val}) ->
   {as_atom(Key), Val};

as_pair(Pair)
 when is_binary(Pair) ->
   [Key, Val] = binary:split(Pair, <<$=>>),
   {as_atom(Key), uri:unescape(Val)}.

as_atom(X) ->
   try scalar:a(uri:unescape(X)) catch _:_ -> undefined end.



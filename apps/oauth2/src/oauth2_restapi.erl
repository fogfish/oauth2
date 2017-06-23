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
%%   common restapi utility
-module(oauth2_restapi).
-compile({parse_transform, category}).
-include("oauth2.hrl").

-export([
   decode/1,
   authenticate/2

   % decode/1,
   % authenticate/2,
   % check_access_code/1,
   % issue_access_code/1,
   % check_pubkey/1,
   % issue_pubkey/1,
   % client_profile/1,
   % redirect_to/1
]).



%%
%% decodes oauth2 request
%% parse application/x-www-form-urlencoded to map
-spec decode(_) -> {ok, _} | {error, _}.  

decode(Request) ->
   {ok, [$. ||
      binary:split(scalar:s(Request), <<$&>>, [trim, global]),
      lists:map(fun as_pair/1, _),
      maps:from_list(_)
   ]}.

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).


%%
%% authenticate client and injects its identity into environment
-spec authenticate(_, _) -> ok | {error, _}.

authenticate(#{<<"client_id">> := Access} = Env, Head) ->
   [either ||
      permit:lookup(Access),
      authenticate_client(_, Env, Head)
   ];

authenticate(Env, Head) ->
   [either ||
      category:maybeT(unauthorized_client,
         lens:get( lens:pair('Authorization', undefined), Head )
      ),
      authenticate_http_digest(_, Env)
   ].

%%
authenticate_client(#{<<"oauth2client">> := <<"public">>}, Env, _) ->
   {ok, Env};
authenticate_client(#{<<"oauth2client">> := <<"confidential">>}, Env, Head) ->
   [either ||
      category:maybeT(unauthorized_client,
         lens:get( lens:pair('Authorization', undefined), Head )
      ),
      authenticate_http_digest(_, Env)
   ].


%%
authenticate_http_digest(<<"Basic ", Digest/binary>>, Env) ->
   [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
   [either ||
      permit:auth(Access, Secret, 3600, #{<<"oauth2client">> => <<"confidential">>}),
      fmap(lens:put(lens:map(<<"client_id">>, undefined), Access, Env)) 
   ];
authenticate_http_digest(_, _) ->
   {error, unauthorized_client}.



%%
%% 2.3. Client Authentication
%%
%% If the client type is confidential, the client and authorization
%% server establish a client authentication method...
%% 
%% The authorization server MAY establish a client authentication method
%% with public clients.
% -spec authenticate(_, _) -> {ok, _} | {error, _}.

% authenticate(Uri, Head) ->
%    case lens:get(lens:pair(<<"client_id">>, undefined), uri:q(Uri)) of
%       undefined ->
%          authenticate_confidential( lens:get(lens:pair('Authorization', undefined), Head) );
      
%       Identity  ->
%          authenticate_public(Identity)
%    end.

% %%
% authenticate_confidential(<<"Basic ", Digest/binary>>) ->
%    [Access, Secret] = binary:split(base64:decode(Digest), <<$:>>),
%    [either ||
%       permit:auth(Access, Secret, 3600, [oauth2client]),
%       oauth2_kvs_client:lookup(Access),
%       is_client_type(<<"confidential">>, _)
%    ];
% authenticate_confidential(_) ->
%    {error, unauthorized}.

% %%
% authenticate_public(Access) ->
%    [either ||
%       oauth2_kvs_client:lookup(Access),
%       is_client_type(<<"public">>, _)
%    ].

% %%
% is_client_type(Type, #{<<"type">> := Type} = Profile) ->
%    {ok, Profile};
% is_client_type(_, _) ->
%    {error, unauthorized}.


 

% %%
% %%
% -spec issue_pubkey(_) -> {ok, _} | {error, _}.

% issue_pubkey(_) ->
%    ok.

% %%
% %% authenticate access/secret key
% -spec check_pubkey(_) -> {ok, _} | {error, _}.

% check_pubkey(#{
%    <<"response_type">> := <<"code">>, 
%    <<"access">> := Access, <<"secret">> := Secret} = Request) ->
%    [either ||
%       permit:auth(Access, Secret, 600),
%       fmap(Request#{<<"access_code">> => _})
%    ];

% check_pubkey(#{
%    <<"response_type">> := <<"token">>, 
%    <<"access">> := Access, <<"secret">> := Secret} = Request) ->
%    [either ||
%       permit:auth(Access, Secret, 3600),
%       fmap(Request#{<<"access_token">> => _})
%    ];

% check_pubkey(_) ->
%    {error, unsupported_response_type}.


% %%
% %%
% -spec client_profile(_) -> {ok, _} | {error, _}.

% client_profile(#{<<"client_id">> := Access} = Request) ->
%    [either ||
%       oauth2_kvs_client:lookup(Access),
%       fmap(maps:merge(_, Request))
%    ];
% client_profile(_) ->
%    {error, invalid_request}.



% %%
% %%
% -spec redirect_to(_) -> {ok, _} | {error, _}. 

% redirect_to(#{
%    <<"type">> := <<"confidential">>, <<"response_type">> := <<"code">>,
%    <<"redirect_uri">> := Uri, <<"access_code">> := Code, <<"state">> := State}) ->

%    Query = [{<<"code">>, Code}, {<<"state">>, State}],
%    {ok, uri:s(uri:q(Query, uri:new(Uri)))};

% redirect_to(#{
%    <<"response_type">> := <<"token">>,
%    <<"redirect_uri">> := Uri, <<"access_token">> := Token, <<"state">> := State}) ->

%    Query = [{<<"access_token">>, Token}, {<<"state">>, State}],
%    {ok, uri:s(uri:q(Query, uri:new(Uri)))}.


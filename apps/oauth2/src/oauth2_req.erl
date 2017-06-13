%% @doc
%%   oauth2 request 
-module(oauth2_req).
-include("oauth2.hrl").
-compile({parse_transform, category}).

-export([
   auth_access_code/1,
   issue_access_code/1,
   auth_client_id/1,
   auth_pubkey/1,
   issue_pubkey/1,
   redirect_uri/1
]).

%%
%% authenticate one-time-use access code to prevent replay.
-spec auth_access_code(_) -> {ok, _} | {error, _}.

auth_access_code(#{<<"access_code">> := Code} = Request) ->
   [either ||
      permit:validate(Code),
      lens:get(lens:map(?OAUTH2_UX), _),
      fmap(Request)
   ].

%%
%%
-spec issue_access_code(_) -> {ok, _} | {error, _}.

issue_access_code(Request) ->
   [either ||
      permit:issue(?OAUTH2_UX, 300),
      fmap(Request#{<<"access_code">> => _})
   ].

%%
%% authenticate client id and its registry profile
-spec auth_client_id(_) -> {ok, _} | {error, _}.

auth_client_id(#{<<"client_id">> := AppId} = Request) ->
   [either ||
      %% @todo: read registry
      permit:issue(AppId, 0),
      fmap(Request#{<<"redirect_uri">> => ?OAUTH2_UX_CALLBACK})
   ].

%%
%% authenticate access/secret key 
-spec auth_pubkey(_) -> {ok, _} | {error, _}. 

auth_pubkey(#{<<"access">> := Access, <<"secret">> := Secret} = Request) ->
   [either ||
      permit:auth(Access, Secret, 600),
      fmap(Request#{<<"access_code">> => _})
   ].

%%
%% issue new pubkey based on defined credentials
-spec issue_pubkey(_) -> {ok, _} | {error, _}. 

issue_pubkey(#{<<"access">> := Access, <<"secret">> := Secret} = Request) ->
   [either ||
      permit:create(Access, Secret),
      fmap(Request#{<<"access_code">> => _})
   ].

%%
%%
-spec redirect_uri(_) -> {ok, _} | {error, _}. 

redirect_uri(#{
   <<"response_type">> := <<"code">>,
   <<"redirect_uri">> := Uri, 
   <<"access_code">> := Code, 
   <<"state">> := State}) ->

   Query = [{<<"code">>, Code}, {<<"state">>, State}],
   {ok, uri:s(uri:q(Query, uri:new(Uri)))}.


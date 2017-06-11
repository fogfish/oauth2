%% @doc
%%   oauth2 request 
-module(oauth2_req).
-compile({parse_transform, category}).

-export([
   access_code/0,
   response_type/0,
   client_id/0,
   redirect_uri/0,
   state/0,
   access/0,
   secret/0,

   define_access_code/1,
   accept_access_code/1,
   define_pubkey/1,
   accept_pubkey/1,
   accept_client_id/1,
   redirect_uri/1
]).


%%
%%
access_code() -> lens:pair(<<"code">>, undefined).
response_type() -> lens:pair(<<"response_type">>, undefined).
client_id() -> lens:pair(<<"client_id">>, undefined).
state() -> lens:pair(<<"state">>, undefined).
redirect_uri() -> lens:pair(<<"redirect_uri">>, undefined).
access() -> lens:pair(<<"access">>, undefined).
secret() -> lens:pair(<<"secret">>, undefined).

%%
%% @todo: make a single use for token
define_access_code(Req) ->
   [either ||
      permit:code(oauth2ux, 300),
      fmap(lens:put(access_code(), _, Req))
   ].   

accept_access_code(Req) ->
   [either ||
      fmap(lens:get(access_code(), Req)),
      permit:validate(_, [oauth2ux]),
      fmap(Req)
   ].

%%
%%
accept_client_id(Req) ->
   [either ||
      fmap(lens:get(client_id(), Req)),
      %% @todo: use client registry to get redirection api
      permit:code(_, 0),
      fmap(lens:put(redirect_uri(), <<"http://localhost:8080/widget/introspect.html">>, Req))
   ].

%%
%%
define_pubkey(Req) ->
   [either ||
      permit:create(
         lens:get(access(), Req),
         lens:get(secret(), Req)
      ),
      fmap(lens:put(access_code(), _, Req))
   ].

accept_pubkey(Req) ->
   [either ||
      permit:auth(
         lens:get(access(), Req),
         lens:get(secret(), Req),
         600
      ),
      fmap(lens:put(access_code(), _, Req))
   ].
   
%%
%%
redirect_uri(Req) ->
   Uri   = lens:get(redirect_uri(), Req),
   Code  = lens:get(access_code(), Req),
   State = lens:get(state(), Req),
   Query = [{<<"code">>, Code}, {<<"state">>, State}],
   {ok, uri:s(uri:q(Query, uri:new(Uri)))}.
   


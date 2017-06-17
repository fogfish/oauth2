-module(oauth2_restapi_client).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'GET'/3,
   'DELETE'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST', 'DELETE'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
     % ,category:maybeT(unauthorized, lens:get(lens:map(<<"oauth2developer">>, undefined)))
   ].

%%
%% 
'GET'(_Type, _Req, {_Uri, Head, Env}) ->
   [either ||
      category:maybeT(badarg, lens:get(lens:pair(<<"id">>), Env))
     ,oauth2_client:lookup(oauth2_restapi:access_token(Head), _)
     ,fmap(jsx:encode(_))
   ].

%%
%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      fmap(permit_oauth2:decode(Req))
     ,fmap(maps:with([<<"redirect_uri">>, <<"type">>], _))
     ,oauth2_restapi:validate_client_type(_)
     ,oauth2_restapi:validate_redirect_uri(_)
     ,oauth2_client:create(oauth2_restapi:access_token(Head), _)
   ].

%%
%%
'DELETE'(_Type, _Req, {_Uri, Head, Env}) ->
   [either ||
      category:maybeT(badarg, uri:unescape(lens:get(lens:pair(<<"id">>), Env)))
     ,oauth2_client:remove(oauth2_restapi:access_token(Head), _)
     ,fmap(jsx:encode(_))
   ].

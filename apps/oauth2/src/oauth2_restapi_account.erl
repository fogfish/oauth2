-module(oauth2_restapi_account).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   authorize/2,
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
   ].

'GET'(_Type, _Req, {_Uri, Head, _Env}) ->
   [either ||
      category:maybeT(unauthorized, oauth2_restapi:access_token(Head))
     ,permit:validate(_)
     ,fmap(lens:get(lens:map(<<"sub">>), _))
     ,oauth2_account:profile(_)
     ,jsx:encode(_)
   ].

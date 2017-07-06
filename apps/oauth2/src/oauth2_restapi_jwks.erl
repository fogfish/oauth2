%%
-module(oauth2_restapi_jwks).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
'GET'(_, _, _) ->
   [either || 
      permit_config:public(),
      jwk:encode(<<"jwt">>, _)
   ].

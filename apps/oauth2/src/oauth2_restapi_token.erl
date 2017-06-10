-module(oauth2_restapi_token).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   authorize/2,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
authorize(_Mthd, {_Uri, Head, _Env}) ->
   case permit_oauth2:authenticate(Head) of
      {error, undefined} ->
         ok;
      {ok, Access} ->
         ok;
      {error, _} = Error ->
         Error
   end.

%%
'POST'(_Type, Req, {_Uri, Head, _Env}) ->
   [either ||
      permit_oauth2:issue_token(Head, Req, 3600),
      fmap(jsx:encode(_))
   ].   

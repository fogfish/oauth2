%% @doc
%%   introspection endpoint
%%   https://tools.ietf.org/html/rfc7662
-module(oauth2_restapi_introspect).
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
%% 2.1. Introspection Request
%%   To prevent token scanning attacks, the endpoint MUST also require
%%   some form of authorization to access this endpoint
authorize(_Mthd, {_Uri, Head, _Env}) ->
   case permit_oauth2:authenticate(Head) of
      {error, undefined} ->
         {error, unauthorized};
      {ok, _Access} ->
         ok;
      {error, _} = Error ->
         Error
   end.

%%
'POST'(_Type, Req, {_Uri, _Head, _Env}) ->
   [either ||
      fmap(permit_oauth2:decode(Req)),
      fmap(lens:get(lens:pair(<<"token">>), _)),
      permit:validate(_),
      fmap(jsx:encode(_))
   ].   

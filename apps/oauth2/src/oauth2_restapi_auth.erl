-module(oauth2_restapi_auth).
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/3,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'POST'].

%%
content_provided(_Req) ->
   [{text, html}].

%%
content_accepted(_Req) ->
   [{application, 'x-www-form-urlencoded'}].

%%
%%
'GET'(_Type, _, {Uri, _Head, _Env}) ->
   case uri:q(Uri) of
      undefined ->
         oauth2_ux:signin([{<<"response_type">>, <<"code">>}, {<<"client_id">>, <<"oauth2ux">>}]);
      Query ->
         oauth2_ux:signin(Query)
   end.

%%
%%
'POST'(_Type, Req, {Uri, Head, Env}) ->
   case 
      [either ||
         fmap(permit_oauth2:decode(Req)),
         oauth2_req:accept_access_code(_),
         oauth2_req:accept_client_id(_),
         oauth2_req:accept_pubkey(_),
         oauth2_req:redirect_uri(_)
      ]
   of
      {ok, Location}   ->
         {302, [{'Location', Location}], <<>>};

      {error, expired} ->
         UriWithError = uri:q(permit_oauth2:decode(Req), Uri),
         'GET'(undefined, undefined, {UriWithError, Head, Env});

      {error, _} = Error  ->
         UriWithError = uri:q([Error | permit_oauth2:decode(Req)], Uri),
         'GET'(undefined, undefined, {UriWithError, Head, Env})
   end.

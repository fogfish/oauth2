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
'GET'(_Type, _, {Uri, _Head, _Env}) ->
   [either ||
      %% @todo: make a single use for token
      permit:code(oauth2ux, 600),
      signin_ux(Uri, _),
      oauth2_signin:ux(_)
   ].

signin_ux(Uri, Token) ->
   {ok, #{
      ux => #{
         access_token  => Token,
         response_type => lens:get(lens:pair(<<"response_type">>), uri:q(Uri)),
         client_id     => lens:get(lens:pair(<<"client_id">>), uri:q(Uri)),
         state         => lens:get(lens:pair(<<"state">>, undefined), uri:q(Uri))
      }
   }}.

%%
'POST'(_Type, Req, {Uri, Head, Env}) ->
   Request = permit_oauth2:decode(Req),
   case 
      [either ||
         validate_access_token(Request),
         validate_client_id(_),
         validate_username(_),
         redirect_to(_)
      ]
   of
      {ok, Location}   ->
         {302, [{'Location', Location}], <<>>};

      {error, expired} ->
         'GET'(undefined, undefined, {uri:q(Request, Uri), Head, Env});         

      {error, _} = Error ->
         Error
   end.


validate_access_token(Request) ->
   [either ||
      permit:validate(
         lens:get(lens:pair(<<"access_token">>), Request),
         [oauth2ux]
      ),
      fmap(Request)
   ].

validate_client_id(Request) ->
   [either ||
      permit:code(
         lens:get(lens:pair(<<"client_id">>), Request),
         0
      ),
      fmap([{<<"redirect_uri">>, <<"http://localhost:8765/path">>} | Request])
   ].

validate_username(Request) ->
   [either ||
      permit:auth(
         lens:get(lens:pair(<<"access">>), Request),
         lens:get(lens:pair(<<"secret">>), Request),
         600
      ),
      fmap([{<<"code">>, _} | Request])
   ].

redirect_to(Request) ->
   Uri   = lens:get(lens:pair(<<"redirect_uri">>), Request),
   Code  = lens:get(lens:pair(<<"code">>), Request),
   State = lens:get(lens:pair(<<"state">>), Request),
   {ok, uri:s(uri:q([{<<"code">>, Code}, {<<"state">>, State}], uri:new(Uri)))}.
   
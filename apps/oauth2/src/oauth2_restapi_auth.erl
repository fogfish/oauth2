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
      signin_ux(uri:q(Uri), _),
      oauth2_signin:ux(_)
   ].

signin_ux(Env, Token) ->
   {ok, [$.||
      fmap(#{ux => #{access_token => Token}}),
      signin_ux_error(Env, _),
      signin_ux_response_type(Env, _),
      signin_ux_client_id(Env, _),
      signin_ux_state(Env, _)
   ]}.

signin_ux_error(Env, Ux) ->
   case lens:get(lens:pair(<<"error">>, undefined), Env) of
      undefined ->
         Ux;
      <<"unauthorized">> ->
         signin_ux_error_message(<<"Invalid credentials.">>, Ux);
      _ ->
         signin_ux_error_message(<<"System error, try later!">>, Ux)
   end.

signin_ux_error_message(Msg, Ux) ->
   lens:put(lens:map(ux), lens:map(error, undefined), Msg, Ux).

signin_ux_response_type(Env, Ux) ->
   lens:put(
      lens:map(ux), lens:map(response_type, undefined),
      lens:get(lens:pair(<<"response_type">>), Env),
      Ux
   ).

signin_ux_client_id(Env, Ux) ->
   lens:put(
      lens:map(ux), lens:map(client_id, undefined),
      lens:get(lens:pair(<<"client_id">>), Env),
      Ux
   ).

signin_ux_state(Env, Ux) ->
   lens:put(
      lens:map(ux), lens:map(state, undefined),
      lens:get(lens:pair(<<"state">>, undefined), Env),
      Ux
   ).


%%
'POST'(_Type, Req, {Uri, Head, Env}) ->
   Request = permit_oauth2:decode(Req),
   io:format("==> ~p~n", [Request]),
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

      {error, Reason}  ->
         'GET'(undefined, undefined, {uri:q([{<<"error">>, Reason} | Request], Uri), Head, Env})
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
   
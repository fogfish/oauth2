%%
%% @doc
%%   authorization request
%%     https://tools.ietf.org/html/rfc6749#section-4.1.1
%%     https://tools.ietf.org/html/rfc6749#section-4.2.1
-module(oauth2_authorize).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").
-include_lib("permit/src/permit.hrl").


-export([
   signup/2
,  signin/2
,  password/2
,  exchange_code/2
]).

%%
%%
-spec signup(#{}, binary()) -> datum:either(uri:uri()).

signup(Headers, Request)
 when is_binary(Request) ->
   req_signup_auth(Headers, lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request))).

%%
req_signup_auth(#{<<"Authorization">> := Digest}, #authorization{redirect_uri = RedirectAddOn} = Request) ->
   [either ||
      #{
         <<"client_id">>    := ClientId,
         <<"redirect_uri">> := RedirectBase
      } <- oauth2_client:confidential(Digest),
      req_signup(
         redirect_uri(RedirectBase, RedirectAddOn),
         Request#authorization{client_id = ClientId}
      )
   ];

req_signup_auth(_, #authorization{client_id = Client, redirect_uri = RedirectAddOn} = Request) ->
   [either ||
      #{<<"redirect_uri">> := RedirectBase} <- oauth2_client:public(Client),
      req_signup(redirect_uri(RedirectBase, RedirectAddOn), Request)
   ].

%%
req_signup(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:create(Access, Secret, Claims),
         exchange_code(_, Claims)
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signup(Redirect, #authorization{
   response_type = <<"token">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State  
}) ->
   case
      [either ||
         permit:create(Access, Secret, Claims),
         permit:revocable(_, 3600, Claims) %% TODO: configurable ttl
      ]
   of
      {ok, Token} ->
         {ok, uri:q([{access_token, Token}, {expires_in, 3600}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signup(_, _) ->
   {error, invalid_request}.

%%
%%
-spec signin(#{}, binary()) -> datum:either(uri:uri()).

signin(Headers, Request)
 when is_binary(Request) ->
   req_signin_auth(Headers, lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request))).

%%
req_signin_auth(#{<<"Authorization">> := Digest}, #authorization{redirect_uri = RedirectAddOn} = Request) ->
   [either ||
      #{
         <<"client_id">>    := ClientId,
         <<"redirect_uri">> := RedirectBase
      } <- oauth2_client:confidential(Digest),
      req_signin(
         redirect_uri(RedirectBase, RedirectAddOn),
         Request#authorization{client_id = ClientId}
      )
   ];

req_signin_auth(_, #authorization{client_id = Client, redirect_uri = RedirectAddOn} = Request) ->
   [either ||
      #{<<"redirect_uri">> := RedirectBase} <- oauth2_client:public(Client),
      req_signin(redirect_uri(RedirectBase, RedirectAddOn), Request)
   ].

req_signin(Redirect, #authorization{
   response_type = <<"code">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      [either ||
         permit:stateless(Access, Secret, 3600, Claims),
         exchange_code(_, Claims)
      ]
   of
      {ok, Code} ->
         {ok, uri:q([{code, Code}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signin(Redirect, #authorization{
   response_type = <<"token">>
,  access = {iri, _, _} = Access
,  secret = Secret
,  scope  = Claims
,  state  = State
}) ->
   case
      permit:revocable(Access, Secret, 3600, Claims) %% TODO: configurable ttl
   of
      {ok, Token} ->
         {ok, uri:q([{access_token, Token}, {expires_in, 3600}, {state, State}], Redirect)};
      {error, Reason} ->
         {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
   end;

req_signin(_, _) ->
   {error, invalid_request}.

%%
%%
-spec password(#{}, binary()) -> datum:either(#{}).

password(Headers, Request)
 when is_binary(Request) ->
   req_password_auth(Headers, lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request))).

req_password_auth(#{<<"Authorization">> := Digest}, Request) ->
   [either ||
      #{
         <<"client_jwt">> := Identity
      } <- oauth2_client:confidential(Digest),
      req_password(Request#authorization{client_id = Identity})
   ];

req_password_auth(_, #authorization{client_id = Client} = Request) ->
   [either ||
      oauth2_client:public(Client),
      req_password(Request)
   ].

req_password(#authorization{
   response_type = <<"password_reset">>
,  client_id = Client
,  redirect_uri = RedirectAddOn
,  access = Access
}) ->
   [either ||
      #pubkey{claims = Claims} <- permit:lookup(Access),
      permit:update(Access, crypto:strong_rand_bytes(30), Claims),
      permit:stateless(_, 3600, #{<<"aud">> => <<"oauth2">>, <<"app">> => <<"password">>}),
      req_reset_link(Client, Access, _),
      oauth2_email:password_reset(Access, _),
      permit:as_access(Client),
      cats:unit(
         uri:q([{client_id, _}, {redirect_uri, RedirectAddOn}],
            uri:path(<<"/oauth2/authorize">>, 
               uri:new(permit_config:iss())
            )
         )
      )
   ];

req_password(#authorization{
   response_type = <<"password_recover">>
,  client_id  = Client
,  redirect_uri = RedirectAddOn
,  state      = Code
,  secret     = Secret
}) ->
   [either ||
      permit:include(Code, #{<<"aud">> => <<"oauth2">>, <<"app">> => <<"password">>}),
      cats:unit(lens:get(lens:at(<<"sub">>), _)),
      Access <- permit:to_access(_),
      #pubkey{claims = Claims} <- permit:lookup(Access),
      permit:update(Access, Secret, Claims),
      #pubkey{claims = #{<<"redirect_uri">> := RedirectBase}} <- permit:lookup(Client),
      exchange_code(Access, Secret, Claims),
      cats:unit(uri:q([{code, _}], redirect_uri(RedirectBase, RedirectAddOn)))
   ].


req_reset_link(Client, Access, Code) ->
   [either ||
      ClientId <- permit:as_access(Client),
      AccessId <- permit:as_access(Access),
      cats:unit(
         uri:anchor(<<"recover">>,
            uri:q([{client_id, ClientId}, {access, AccessId}, {code, Code}],
               uri:path(<<"/oauth2/authorize">>, 
                  uri:new(permit_config:iss())
               )
            )
         )
      )
   ].

%%
%%
exchange_code(Token, Claims) ->
   %% TODO: configurable ttl
   permit:stateless(Token, 3600, #{
      <<"aud">> => <<"oauth2">>
   ,  <<"app">> => base64url:encode(jsx:encode(Claims))
   }).

exchange_code(Access, Secret, Claims) ->
   %% TODO: configurable ttl
   permit:stateless(Access, Secret, 3600, #{
      <<"aud">> => <<"oauth2">>
   ,  <<"app">> => base64url:encode(jsx:encode(Claims))
   }).

%%
%%
redirect_uri(Base, undefined) ->
   uri:new(Base);
redirect_uri(Base, <<>>) ->
   uri:new(Base);
redirect_uri(Base, AddOn) ->
   Uri = uri:new(Base),
   uri:segments(uri:segments(Uri) ++ only_ascii(uri:segments(uri:new(AddOn))), Uri).

only_ascii(Segments) ->
   lists:filter(
      fun(X) -> X /= <<>> end,
      lists:map(
         fun(Segment) ->
            << <<C:8>> || <<C:8>> <= Segment, is_ascii(C)>>
         end,
         Segments
      )
   ).

is_ascii(X) when X >= $0 andalso X =< $9 -> true;
is_ascii(X) when X >= $A andalso X =< $Z -> true;
is_ascii(X) when X >= $a andalso X =< $z -> true;
is_ascii(_) -> false.
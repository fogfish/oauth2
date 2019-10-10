-module(oauth2).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").

-export([
   signup/2
,  signin/2
,  token/1
]).

%%
%%
-spec signup(#{}, binary()) -> datum:either(uri:uri()).

signup(Headers, Request) ->
   oauth2_authorize:signup(Headers, Request).

%%
%%
-spec signin(#{}, binary()) -> datum:either(uri:uri()).

signin(Headers, Request) ->
   oauth2_authorize:signin(Headers, Request).


% signup(Request)
%  when is_binary(Request) ->
%    signup(lens:get(oauth2_codec:authorization(), oauth2_codec:decode(Request)));

% signup(#authorization{client_id = {iri, _, _} = Client} = Request) ->
%    [either ||
%       permit:lookup(Client),
%       #{<<"redirect_uri">> := Redirect} <- permit:include(_, #{}),
%       signup(uri:new(Redirect), Request)
%    ].

% signup(Redirect, #authorization{
%    response_type = <<"code">>
% ,  access = {iri, _, _} = Access
% ,  secret = Secret
% ,  scope  = Claims
% ,  state  = State
% }) ->
%    case
%       [either ||
%          permit:create(Access, Secret, Claims),
%          exchange_code(_, Claims)
%       ]
%    of
%       {ok, Code} ->
%          {ok, uri:q([{code, Code}, {state, State}], Redirect)};
%       {error, Reason} ->
%          {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
%    end;

% signup(Redirect, #authorization{
%    response_type = <<"token">>
% ,  access = {iri, _, _} = Access
% ,  secret = Secret
% ,  scope  = Claims
% ,  state  = State  
% }) ->
%    case
%       [either ||
%          permit:create(Access, Secret, Claims),
%          permit:revocable(_, 3600, Claims) %% TODO: configurable ttl
%       ]
%    of
%       {ok, Token} ->
%          {ok, uri:q([{access_token, Token}, {expires_in, 3600}, {state, State}], Redirect)};
%       {error, Reason} ->
%          {ok, uri:q([{error, Reason}, {state, State}], Redirect)}
%    end;

% signup(_, _) ->
%    {error, invalid_request}.

exchange_code(Token, Claims) ->
   %% TODO: configurable ttl
   permit:stateless(Token, 3600, #{
      <<"aud">> => <<"oauth2">>
   ,  <<"app">> => base64url:encode(jsx:encode(Claims))
   }).

%%
%%
token(Request)
 when is_binary(Request) ->
   token(lens:get(oauth2_codec:access_token(), oauth2_codec:decode(Request)));

token(#access_token{
   grant_type = <<"authorization_code">>
,  client_id  = {iri, _, _} = Client
,  code       = Code
}) ->
   [either ||
      permit:include(Code, #{<<"aud">> => <<"oauth2">>}),
      #{<<"app">> := Encoded} <- permit:equals(Code, #{}),
      Claims  <- cats:unit(jsx:decode(base64url:decode(Encoded), [return_maps])),
      Access  <- permit:revocable(Code, 3600, Claims), %% TODO: configurable ttl 
      Refresh <- exchange_code(Code, Claims),
      cats:unit(
         maps:merge(Claims,
            #{
               <<"token_type">>    => <<"bearer">>, 
               <<"expires_in">>    => 3600,
               <<"access_token">>  => Access,
               <<"refresh_token">> => Refresh
            }
         )
      )
   ];

token(#access_token{
   grant_type = <<"password">>
}) ->
   ok;

token(#access_token{
   grant_type = <<"password">>
}) ->
   ok;

token(#access_token{
   grant_type = <<"client_credentials">>
}) ->
   ok;

token(#access_token{
   grant_type = <<"refresh_token">>
}) ->
   ok;

token(_) ->
   {error, invalid_request}.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%%
iri(Access) ->
   case binary:split(Access, <<$@>>) of
      [Suffix, Prefix] ->
         {ok, {iri, Prefix, Suffix}};
      _ ->
         {error, {badarg, Access}}
   end.

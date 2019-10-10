%%
%% @doc
%%   access token request
%%   https://tools.ietf.org/html/rfc6749#section-4.1.3
-module(oauth2_access).

-compile({parse_transform, category}).
-include_lib("include/oauth2.hrl").

% -export([token/2]).

% %%
% %%
% -spec token(#{}, binary()) -> datum:either(#{}).

% token(Headers, Request)
%  when is_binary(Request) ->
%    req_token_auth(Headers, lens:get(oauth2_codec:access_token(), oauth2_codec:decode(Request))).

% %%
% req_token_auth(#{<<"Authorization">> := Digest}, Request) ->
%    [either ||
%       % #{<<"redirect_uri">> := Redirect} <- oauth2_client:confidential(Digest),
%       % req_signup(uri:new(Redirect), Request)
%    ];

% req_token_auth(_, #authorization{client_id = Client} = Request) ->
%    [either ||
%       % #{<<"redirect_uri">> := Redirect} <- oauth2_client:public(Client),
%       % req_signup(uri:new(Redirect), Request)
%    ].


% %%
% %%
% token(Request)
%  when is_binary(Request) ->
%    token(lens:get(oauth2_codec:access_token(), oauth2_codec:decode(Request)));

% token(#access_token{
%    grant_type = <<"authorization_code">>
% ,  client_id  = {iri, _, _} = Client
% ,  code       = Code
% }) ->
%    [either ||
%       permit:include(Code, #{<<"aud">> => <<"oauth2">>}),
%       #{<<"app">> := Encoded} <- permit:equals(Code, #{}),
%       Claims  <- cats:unit(jsx:decode(base64url:decode(Encoded), [return_maps])),
%       Access  <- permit:revocable(Code, 3600, Claims), %% TODO: configurable ttl 
%       Refresh <- oauth2_authorize:exchange_code(Code, Claims),
%       cats:unit(
%          maps:merge(Claims,
%             #{
%                <<"token_type">>    => <<"bearer">>, 
%                <<"expires_in">>    => 3600,
%                <<"access_token">>  => Access,
%                <<"refresh_token">> => Refresh
%             }
%          )
%       )
%    ];

% token(#access_token{
%    grant_type = <<"password">>
% }) ->
%    ok;

% token(#access_token{
%    grant_type = <<"password">>
% }) ->
%    ok;

% token(#access_token{
%    grant_type = <<"client_credentials">>
% }) ->
%    ok;

% token(#access_token{
%    grant_type = <<"refresh_token">>
% }) ->
%    ok;

% token(_) ->
%    {error, invalid_request}.

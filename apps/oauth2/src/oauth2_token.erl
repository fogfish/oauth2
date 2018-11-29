-module(oauth2_token).
-compile({parse_transform, category}).

-export([
   bearer/1,
   bearer/2,
   access/1,
   access/2,
   refresh/1,
   exchange_code/2,
   is_exchangable/1,
   recovery/1,
   is_recoverable/1
]).


%%
%% issue new bearer token, using input token as reference
bearer(Token) ->
   [either ||
      AccessT <- oauth2_token:access(Token),
      Refresh <- oauth2_token:refresh(Token),
      cats:unit(#{
         <<"token_type">> => <<"bearer">>, 
         <<"expires_in">> => ttl_access_token(),
         <<"access_token">>  => AccessT,
         <<"refresh_token">> => Refresh
      })
   ].

bearer(Access, Secret) ->
   [either ||
      AccessT  <- oauth2_token:access(Access, Secret),
      Refresh <- oauth2_token:refresh(AccessT),
      cats:unit(#{
         <<"token_type">> => <<"bearer">>, 
         <<"expires_in">> => ttl_access_token(),
         <<"access_token">>  => AccessT,
         <<"refresh_token">> => Refresh
      })
   ].


%%
%% issue new access token
access(Token) ->
   permit:revocable(
      Token, 
      ttl_access_token(),
      permit:default_claims()
   ).

access(Access, Secret) ->
   permit:revocable(
      Access, 
      Secret, 
      ttl_access_token(),
      permit:default_claims()
   ).

%%
%% issue new refresh token
refresh(Token) ->
   permit:revocable(
      Token, 
      ttl_refresh_token(), 
      #{<<"exch">> => true}
   ).

%%
%% issue new exchange code
exchange_code(Access, Secret) ->
   permit:stateless(
      Access,
      Secret,
      ttl_exchange_code(),
      #{<<"exch">> => true}
   ).

%%
%% 
is_exchangable(Token) ->
   permit:include(Token, #{<<"exch">> => true}).

%%
%% issue new recovery token
recovery(Token) ->
   permit:revocable(
      Token,
      ttl_recovery_code(),
      #{<<"recovery">> => true}
   ).

is_recoverable(Token) ->
   permit:include(Token, #{<<"recovery">> => true}).

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------


%%
%%
ttl_access_token() ->
   scalar:i(opts:val(ttl_access_token, 1200, oauth2)).

%%
%%
ttl_refresh_token() ->
   scalar:i(opts:val(ttl_refresh_token, 86400, oauth2)).

%%
%%
ttl_exchange_code() ->
   scalar:i(opts:val(ttl_exchange_code, 60, oauth2)).

%%
%%
ttl_recovery_code() ->
   scalar:i(opts:val(ttl_recovery_code, 300, oauth2)).


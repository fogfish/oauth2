-module(oauth2_client).
-compile({parse_transform, category}).

%%
%% interface
-export([
   create/2,
   lookup/2,
   remove/2
]).

%%
%%
-spec create(permit:token(), map()) -> {ok, map()} | {error, _}.

create(Token, _Client) ->
   [either ||
      permit:pubkey(Token, [oauth2client]),
      % create_app(_, Spec),
      fmap(jsx:encode(_))
   ].

% create_app(#{<<"access">> := Access}, Spec) ->
%    pts:put(oauth2profile, Access, Spec#{<<"access">> => Access}).      

%%
%%
-spec lookup(permit:token(), permit:access()) -> {ok, _} | {error, _}.

lookup(_Token, _Access) ->
   {ok, #{}}.

%%
%%
-spec remove(permit:token(), permit:access()) -> {ok, _} | {error, _}.

remove(_Token, Access) ->
   %% find user by id from token
   %% validate that user owns resource
   %% delete the resource
   [either ||
      permit:revoke(Access),
      fmap(#{<<"access">> => Access})
   ].

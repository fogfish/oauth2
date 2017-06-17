%% @doc
%%   account management 
-module(oauth2_account).
-compile({parse_transform, category}).

-export([
   profile/1
]).


%%
%%
-spec profile(permit:access()) -> {ok, map()} | {error, _}.

profile(Access) ->
   [either ||
      permit:lookup(Access),
      fmap(maps:with([<<"access">>, <<"roles">>], _)),
      lookup_clients(_)
   ].   

lookup_clients(#{<<"access">> := Access} = Profile) ->
   [either ||
      pts:call(permit, Access, {match, <<"master">>, [{<<"master">>, Access}]}),
      fmap(Profile#{<<"clients">> => _})
   ].


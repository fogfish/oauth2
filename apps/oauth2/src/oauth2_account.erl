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
      lookup_clients_info(_),
      fmap(Profile#{<<"clients">> => _})
   ].

lookup_clients_info(List) ->
   {ok, lists:map(
      fun(#{<<"access">> := Access} = Basic) ->
         {ok, Client} = oauth2_client:lookup(Access),
         maps:merge(Basic, Client)
      end,
      List
   )}.

-module(client_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-compile(export_all).
-compile({parse_transform, category}).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   os:putenv("PERMIT_ISSUER", "https://example.com"),
   os:putenv("PERMIT_AUDIENCE", "suite"),
   os:putenv("PERMIT_CLAIMS", "read=true&write=true"),
   {ok, _} = application:ensure_all_started(oauth2),
   permit:create({iri, <<"org">>, <<"joe">>}, <<"secret">>, #{}),
   Config.

end_per_suite(_Config) ->
   ok.

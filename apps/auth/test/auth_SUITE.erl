-module(auth_SUITE).

-export([all/0]).
-export([test/1]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

test(_) ->
   serverless:mock(auth, 
      #{},
      #{}
   ).

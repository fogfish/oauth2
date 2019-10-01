-module(oauth2_codec_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("oauth2/include/oauth2.hrl").
-compile({parse_transform, generic}).

-export([all/0]).
-compile(export_all).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

authorization(_) ->
   Request = <<"response_type=code&client_id=public@org&access=access@org&secret=secret&scope=read%3Dtrue%26write%3Dtrue">>,
   Generic = oauth2_codec:decode(Request),
   #authorization{
      response_type  = <<"code">>
   ,  client_id      = {iri, <<"org">>, <<"public">>}
   ,  access         = {iri, <<"org">>, <<"access">>}
   ,  secret         = <<"secret">>
   ,  scope          = #{<<"read">> := <<"true">>, <<"write">> := <<"true">>}
   } = lens:get(oauth2_codec:authorization(), Generic).

-module(oauth2_SUITE).
-include_lib("common_test/include/ct.hrl").

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
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   os:putenv("PERMIT_ISSUER", "http://example.com"),
   os:putenv("PERMIT_AUDIENCE", "suite"),
   os:putenv("PERMIT_CLAIMS", "read=true&write=true"),
   {ok, _} = application:ensure_all_started(oauth2),
   {ok, _} = permit:create({iri, <<"org">>, <<"public">>}, <<"secret">>, client_spec_public()),
   {ok, _} = permit:create({iri, <<"org">>, <<"confidential">>}, <<"secret">>, client_spec_confidential()),

   Config.


end_per_suite(_Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% fixtures
%%%
%%%----------------------------------------------------------------------------   

client_spec_public() ->
   #{
      <<"security">> => <<"public">>,
      <<"redirect_uri">> => <<"http://example.com/public">>
   }.

client_spec_confidential() ->
   #{
      <<"security">> => <<"confidential">>,
      <<"redirect_uri">> => <<"http://example.com/confidential">>
   }.


%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

auth_public_client(_) ->
   Expect = client_spec_public(),
   {ok, Expect} = oauth2:auth_public_client(<<"org.public">>),
   {error, forbidden} = oauth2:auth_public_client(<<"org.confidential">>),
   {error, not_found} = oauth2:auth_public_client(<<"org.undefined">>).
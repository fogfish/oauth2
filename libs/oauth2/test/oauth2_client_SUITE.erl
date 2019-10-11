-module(oauth2_client_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("oauth2/include/oauth2.hrl").

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
   os:putenv("PERMIT_ISSUER", "https://example.com"),
   os:putenv("PERMIT_AUDIENCE", "suite"),
   os:putenv("PERMIT_CLAIMS", "read=true&write=true"),
   {ok, _} = application:ensure_all_started(permit),
   erlang:apply(permit, create, oauth2_FIXTURES:client_public()),
   erlang:apply(permit, create, oauth2_FIXTURES:client_confidential()),
   Config.


end_per_suite(_Config) ->
   application:stop(permit),
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

%%
auth_client_public(_) ->
   [_, _, Expect] = oauth2_FIXTURES:client_public(),
   {ok, Expect} = oauth2_client:public(<<"public@org">>),
   {error, forbidden} = oauth2_client:public(<<"confidential@org">>),
   {error, not_found} = oauth2_client:public(<<"undefined@org">>).

%%
auth_client_default(_) ->
   [_, _, Expect] = oauth2_FIXTURES:client_default(),
   {ok, Expect} = oauth2_client:public(<<"account@oauth2">>),
   {ok, Expect} = oauth2_client:public(<<"account@oauth2">>).

%%
auth_client_confidential(_) ->
   [_, _, Expect] = oauth2_FIXTURES:client_confidential(),
   {ok, Spec} = oauth2_client:confidential(digest(<<"confidential@org">>, <<"secret">>)),
   Expect = maps:without([<<"client_id">>, <<"client_jwt">>], Spec),
   {error,unauthorized} = oauth2_client:confidential(digest(<<"confidential@org">>, <<"undefined">>)),
   {error, forbidden} = oauth2_client:confidential(digest(<<"public@org">>, <<"secret">>)),
   {error, not_found} = oauth2_client:confidential(digest(<<"undefined@org">>, <<"secret">>)).

digest(Access, Secret) ->
   <<"Basic ", (base64:encode(<<Access/binary, $:, Secret/binary>>))/binary>>.

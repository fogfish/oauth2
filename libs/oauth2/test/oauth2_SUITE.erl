-module(oauth2_SUITE).
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
      <<"redirect_uri">> => <<"https://example.com/public">>
   }.

client_spec_default() ->
   #{
      <<"security">> => <<"public">>,
      <<"redirect_uri">> => <<"https://example.com/oauth2/account">>
   }.

client_spec_confidential() ->
   #{
      <<"security">> => <<"confidential">>,
      <<"redirect_uri">> => <<"https://example.com/confidential">>
   }.


%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

%%
auth_client_public(_) ->
   Expect = client_spec_public(),
   {ok, Expect} = oauth2:auth_client_public(<<"public@org">>),
   {error, forbidden} = oauth2:auth_client_public(<<"confidential@org">>),
   {error, not_found} = oauth2:auth_client_public(<<"undefined@org">>).

%%
auth_client_default(_) ->
   Expect = client_spec_default(),
   {ok, Expect} = oauth2:auth_client_public(<<"account@oauth2">>).

%%
auth_client_confidential(_) ->
   Expect = client_spec_confidential(), 
   {ok, Expect} = oauth2:auth_client_confidential(digest(<<"confidential@org">>, <<"secret">>)),
   {error,unauthorized} = oauth2:auth_client_confidential(digest(<<"confidential@org">>, <<"undefined">>)),
   {error, forbidden} = oauth2:auth_client_confidential(digest(<<"public@org">>, <<"secret">>)),
   {error, not_found} = oauth2:auth_client_confidential(digest(<<"undefined@org">>, <<"secret">>)).

digest(Access, Secret) ->
   <<"Basic ", (base64:encode(<<Access/binary, $:, Secret/binary>>))/binary>>.

%%
signup(_) ->
   Request = <<"response_type=code&client_id=public@org&access=access@org&secret=secret&scope=read%3Dtrue%26write%3Dtrue">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   Code = uri:q(<<"code">>, undefined, Uri),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := <<"org">>
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   ,  <<"sub">> := {iri, <<"org">>, <<"access">>}
   }} = permit:validate(Code),
   {ok, #{}} = permit:equals(Code, #{}).

%%
signup_conflict(_) ->
   Request = <<"response_type=code&client_id=public@org&access=public@org&secret=secret&scope=read%3Dtrue%26write%3Dtrue">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"conflict">> = uri:q(<<"error">>, undefined, Uri).

%%
signup_client_unknown(_) ->
   Request = <<"response_type=code&client_id=unknown@org&access=access@org&secret=secret&scope=read%3Dtrue%26write%3Dtrue">>,
   {error, not_found} = oauth2:signup(Request).


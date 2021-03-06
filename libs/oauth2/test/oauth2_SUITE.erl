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
      Test =/= access,
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
   os:putenv("PERMIT_CLAIMS", "rd=none&wr=none"),
   {ok, _} = application:ensure_all_started(oauth2),
   erlang:apply(permit, create, oauth2_FIXTURES:client_public()),
   erlang:apply(permit, create, oauth2_FIXTURES:client_confidential()),
   erlang:apply(permit, create, oauth2_FIXTURES:joe()),
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
digest() ->
   #{<<"Authorization">> => <<"Basic ", (base64:encode(<<"confidential@org:secret">>))/binary>>}.

access(Suffix) ->
   Prefix = base64url:encode(crypto:hash(md5, Suffix)),
   {iri, Prefix, Suffix}.

%%
signup_code_public(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe.c.p@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(#{}, Request),
   ct:pal("==> ~p~n", [Uri]),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   authorization_code_check(access(<<"joe.c.p@org">>), Uri).

%%
signin_code_public(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   authorization_code_check(access(<<"joe@org">>), Uri).

%%
signup_code_confidential(_) ->
   Request = <<"response_type=code&access=joe.c.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   authorization_code_check(access(<<"joe.c.c@org">>), Uri).

%%
signin_code_confidential(_) ->
   Request = <<"response_type=code&access=joe.c.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   authorization_code_check(access(<<"joe.c.c@org">>), Uri).

authorization_code_check({iri, IDP, _} = Access, Uri) ->
   Code = uri:q(<<"code">>, undefined, Uri),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"oauth2">>
   ,  <<"idp">> := IDP
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   ,  <<"sub">> := Access
   }} = permit:validate(Code),
   {ok, _} = permit:equals(Code, #{}).

%%
signup_implicit_public(_) ->
   Request = <<"response_type=token&client_id=public@org&access=joe.i.p@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   access_token_check(access(<<"joe.i.p@org">>), Uri).

%%
signin_implicit_public(_) ->
   Request = <<"response_type=token&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   access_token_check(access(<<"joe@org">>), Uri).

%%
signup_implicit_confidential(_) ->
   Request = <<"response_type=token&access=joe.i.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   access_token_check(access(<<"joe.i.c@org">>), Uri).

%%
signin_implicit_confidential(_) ->
   Request = <<"response_type=token&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   access_token_check(access(<<"joe@org">>), Uri).

access_token_check({iri, IDP, _} = Access, Uri) ->
   Code = uri:q(<<"access_token">>, undefined, Uri),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := IDP
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   ,  <<"sub">> := Access
   }} = permit:validate(Code),
   {ok, _} = permit:equals(Code, #{<<"rd">> => <<"api">>, <<"wr">> => <<"ddb">>}).

%%
signup_conflict_code(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"conflict">> = uri:q(<<"error">>, undefined, Uri).

%%
signup_conflict_implicit(_) ->
   Request = <<"response_type=token&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"conflict">> = uri:q(<<"error">>, undefined, Uri).

%%
signup_unknown_public(_) ->
   Request = <<"response_type=code&client_id=unknown@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {error, not_found} = oauth2:signup(#{}, Request).

%%
signin_unknown_public(_) ->
   Request = <<"response_type=code&client_id=unknown@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {error, not_found} = oauth2:signin(#{}, Request).

%%
signup_unknown_confidential(_) ->
   Request = <<"response_type=code&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {error, not_found} = oauth2:signup(
      #{<<"Authorization">> => <<"Basic ", (base64:encode(<<"unknown@org:secret">>))/binary>>},
      Request
   ).

%%
signin_unknown_confidential(_) ->
   Request = <<"response_type=code&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {error, not_found} = oauth2:signin(
      #{<<"Authorization">> => <<"Basic ", (base64:encode(<<"unknown@org:secret">>))/binary>>},
      Request
   ).

%%
signin_not_found_code(_) ->
   Request = <<"response_type=code&client_id=public@org&access=unknown@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"not_found">> = uri:q(<<"error">>, undefined, Uri).

%%
signin_not_found_implicit(_) ->
   Request = <<"response_type=token&client_id=public@org&access=unknown@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"not_found">> = uri:q(<<"error">>, undefined, Uri).

%%
signin_escalation_attack_code(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dall">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"forbidden">> = uri:q(<<"error">>, undefined, Uri).

%%
signin_escalation_attack_implicit(_) ->
   Request = <<"response_type=token&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dall">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   <<"forbidden">> = uri:q(<<"error">>, undefined, Uri).

%%
%%
access_token_code_public(_) ->
   RequestA = <<"response_type=code&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, RequestA),
   Code = uri:q(<<"code">>, undefined, Uri),
   RequestB = <<"grant_type=authorization_code&client_id=public@org&code=", Code/binary>>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   ,  <<"rd">>           := <<"api">>
   ,  <<"wr">>           := <<"ddb">>
   } = AccessToken } = oauth2:token(#{}, RequestB),
   false = maps:is_key(<<"refresh_token">>, AccessToken).

%%
%%
access_token_code_confidential(_) ->
   RequestA = <<"response_type=code&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), RequestA),
   Code = uri:q(<<"code">>, undefined, Uri),
   RequestB = <<"grant_type=authorization_code&code=", Code/binary>>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   ,  <<"refresh_token">>:= _
   ,  <<"rd">>           := <<"api">>
   ,  <<"wr">>           := <<"ddb">>
   }} = oauth2:token(digest(), RequestB).

%%
%%
access_token_password_public(_) ->
   Request = <<"grant_type=password&client_id=public@org&username=joe@org&password=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   ,  <<"rd">>           := <<"api">>
   ,  <<"wr">>           := <<"ddb">>
   } = AccessToken } = oauth2:token(#{}, Request),
   false = maps:is_key(<<"refresh_token">>, AccessToken).

%%
%%
access_token_password_confidential(_) ->
   Request = <<"grant_type=password&username=joe@org&password=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   ,  <<"refresh_token">>:= _
   ,  <<"rd">>           := <<"api">>
   ,  <<"wr">>           := <<"ddb">>
   }} = oauth2:token(digest(), Request).

%%
%%
access_token_public(_) ->
   Request = <<"grant_type=client_credentials&client_id=public@org">>,
   {error, invalid_request} = oauth2:token(#{}, Request).

%%
%%
access_token_confidential(_) ->
   Request = <<"grant_type=client_credentials">>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   } = AccessToken} = oauth2:token(digest(), Request),
   false = maps:is_key(<<"refresh_token">>, AccessToken).

%%
%%
refresh_token_public(_) ->
   Request = <<"grant_type=refresh_token&refresh_token=xxx&client_id=public@org">>,
   {error, invalid_request} = oauth2:token(#{}, Request).

%%
%%
refresh_token_confidential(_) ->
   RequestA = <<"response_type=code&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), RequestA),
   Code = uri:q(<<"code">>, undefined, Uri),
   RequestB = <<"grant_type=authorization_code&code=", Code/binary>>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"refresh_token">>:= Token
   }} = oauth2:token(digest(), RequestB),
   RequestC = <<"grant_type=refresh_token&refresh_token=", Token/binary>>,
   {ok, #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"expires_in">>   := _
   ,  <<"access_token">> := _
   ,  <<"refresh_token">>:= _
   ,  <<"rd">>           := <<"api">>
   ,  <<"wr">>           := <<"ddb">>
   }} = oauth2:token(digest(), RequestC).

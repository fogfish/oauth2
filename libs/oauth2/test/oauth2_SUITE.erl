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

%%
signup_code_public(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe.c.p@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   authorization_code_check({iri, <<"org">>, <<"joe.c.p">>}, Uri).

%%
signin_code_public(_) ->
   Request = <<"response_type=code&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   authorization_code_check({iri, <<"org">>, <<"joe">>}, Uri).

%%
signup_code_confidential(_) ->
   Request = <<"response_type=code&client_id=confidential@org&access=joe.c.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   authorization_code_check({iri, <<"org">>, <<"joe.c.c">>}, Uri).

%%
signin_code_confidential(_) ->
   Request = <<"response_type=code&client_id=confidential@org&access=joe.c.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   authorization_code_check({iri, <<"org">>, <<"joe.c.c">>}, Uri).

authorization_code_check(Access, Uri) ->
   Code = uri:q(<<"code">>, undefined, Uri),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"oauth2">>
   ,  <<"idp">> := <<"org">>
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
   access_token_check({iri, <<"org">>, <<"joe.i.p">>}, Uri).

%%
signin_implicit_public(_) ->
   Request = <<"response_type=token&client_id=public@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(#{}, Request),
   <<"example.com">> = uri:host(Uri),
   <<"/public">> = uri:path(Uri),
   access_token_check({iri, <<"org">>, <<"joe">>}, Uri).

%%
signup_implicit_confidential(_) ->
   Request = <<"response_type=token&client_id=confidential@org&access=joe.i.c@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signup(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   access_token_check({iri, <<"org">>, <<"joe.i.c">>}, Uri).

%%
signin_implicit_confidential(_) ->
   Request = <<"response_type=token&client_id=confidential@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {ok, {uri, https, _} = Uri} = oauth2:signin(digest(), Request),
   <<"example.com">> = uri:host(Uri),
   <<"/confidential">> = uri:path(Uri),
   access_token_check({iri, <<"org">>, <<"joe">>}, Uri).

access_token_check(Access, Uri) ->
   Code = uri:q(<<"access_token">>, undefined, Uri),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := <<"org">>
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
   Request = <<"response_type=code&client_id=unknown@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
   {error, not_found} = oauth2:signup(
      #{<<"Authorization">> => <<"Basic ", (base64:encode(<<"unknown@org:secret">>))/binary>>},
      Request
   ).

%%
signin_unknown_confidential(_) ->
   Request = <<"response_type=code&client_id=unknown@org&access=joe@org&secret=secret&scope=rd%3Dapi%26wr%3Dddb">>,
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

% %%
% token_gt_authorization_code(_) ->
%    Claims = #{<<"read">> => <<"true">>},
%    {ok, Code} = permit:stateless(
%       {iri, <<"org">>, <<"user">>},
%       <<"secret">>,
%       3600,
%       #{
%          <<"aud">> => <<"oauth2">>
%       ,  <<"app">> => base64url:encode(jsx:encode(Claims))
%       }
%    ),
%    Request = <<"grant_type=authorization_code&client_id=public@org&code=", Code/binary>>,
%    {ok, #{
%       <<"token_type">>   := <<"bearer">>
%    ,  <<"expires_in">>   := _
%    ,  <<"access_token">> := _
%    ,  <<"refresh_token">>:= _
%    ,  <<"read">>         := <<"true">>
%    }} = oauth2:token(Request).

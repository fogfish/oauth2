-module(authorize_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
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
   {ok, _} = permit:create({iri, <<"org">>, <<"confidential">>}, <<"secret">>),
   {ok, _} = permit:create({iri, <<"org">>, <<"access">>}, <<"secret">>),
   Config.


end_per_suite(_Config) ->
   ok.

%%
%%
signup_public_client(_) ->
   #{
      statusCode := 302
   ,  headers := #{
         <<"Location">> := Location
      }
   ,  body := _
   } = serverless:mock(authorize,
      #{
         <<"httpMethod">> => <<"POST">>
      ,  <<"path">> => <<"/signup">>
      ,  <<"headers">> => #{
            <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
         }
      ,  <<"body">> => <<"response_type=code&client_id=account@oauth2&access=signup-public@org&secret=secret">>
      }
   ),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := <<"org">>
   ,  <<"sub">> := {iri, <<"org">>, <<"signup-public">>}
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   }} = permit:validate( uri:q(<<"code">>, undefined, uri:new(Location)) ).

%%
%%
signin_public_client(_) ->
   #{
      statusCode := 302
   ,  headers := #{
         <<"Location">> := Location
      }
   ,  body := _
   } = serverless:mock(authorize,
      #{
         <<"httpMethod">> => <<"POST">>
      ,  <<"path">> => <<"/signin">>
      ,  <<"headers">> => #{
            <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
         }
      ,  <<"body">> => <<"response_type=code&client_id=account@oauth2&access=access@org&secret=secret">>
      }
   ),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := <<"org">>
   ,  <<"sub">> := {iri, <<"org">>, <<"access">>}
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   }} = permit:validate( uri:q(<<"code">>, undefined, uri:new(Location)) ).

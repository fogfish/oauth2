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
   {ok, _} = application:ensure_all_started(oauth2),
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
      ,  <<"body">> => <<"response_type=code&client_id=account@oauth2&access=joe@org&secret=secret">>
      }
   ),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"oauth2">>
   ,  <<"idp">> := <<"org">>
   ,  <<"sub">> := {iri, <<"org">>, <<"joe">>}
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
      ,  <<"body">> => <<"response_type=code&client_id=account@oauth2&access=joe@org&secret=secret">>
      }
   ),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"oauth2">>
   ,  <<"idp">> := <<"org">>
   ,  <<"sub">> := {iri, <<"org">>, <<"joe">>}
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   }} = permit:validate( uri:q(<<"code">>, undefined, uri:new(Location)) ).

-module(token_SUITE).

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
   IDP = base64url:encode(crypto:hash(md5, <<"joe@org">>)),
   permit:create({iri, IDP, <<"joe@org">>}, <<"secret">>, #{}),
   Config.

end_per_suite(_Config) ->
   ok.

%%
%%
code_exchange(_) ->
   IDP = base64url:encode(crypto:hash(md5, <<"joe@org">>)),
   {ok, Code} = [either ||
      permit:stateless({iri, IDP, <<"joe@org">>}, <<"secret">>, 10, #{}),
      oauth2_authorize:exchange_code(_, #{})
   ],
   #{
      statusCode := 200
   ,  body := Json
   } = serverless:mock(token,
      #{
         <<"httpMethod">> => <<"POST">>
      ,  <<"path">> => <<"/oauth2/token">>
      ,  <<"headers">> => #{
            <<"content-type">> => <<"application/x-www-form-urlencoded">>
         }
      ,  <<"body">> => <<"grant_type=authorization_code&client_id=account@oauth2&code=", Code/binary>>
      }
   ),
   #{
      <<"token_type">>   := <<"bearer">>
   ,  <<"access_token">> := AccessToken
   ,  <<"expires_in">>   := _
   ,  <<"aud">>          := <<"suite">>
   ,  <<"idp">>          := IDP
   ,  <<"iss">>          := <<"https://example.com">>
   ,  <<"sub">>          := <<"joe@org">>
   ,  <<"exp">>          := _
   ,  <<"tji">>          := _
   } = jsx:decode(Json, [return_maps]),
   {ok, #{
      <<"iss">> := <<"https://example.com">>
   ,  <<"aud">> := <<"suite">>
   ,  <<"idp">> := IDP
   ,  <<"sub">> := {iri, IDP, <<"joe@org">>}
   ,  <<"exp">> := _
   ,  <<"tji">> := _
   ,  <<"rev">> := true
   }} = permit:validate( AccessToken ).

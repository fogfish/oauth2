-module(oauth2_FIXTURES).

-export([
   client_default/0
,  client_public/0
,  client_confidential/0
]).

%%
client_default() ->
   [
      {iri, <<"oauth2">>, <<"account">>}
   ,  undefined
   ,  #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"https://example.com/oauth2/account">>
      }
   ].

%%
client_public() ->
   [
      {iri, <<"org">>, <<"public">>}
   ,  <<"secret">>
   ,  #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"https://example.com/public">>
      }
   ].

%%
client_confidential() ->
   [
      {iri, <<"org">>, <<"confidential">>}
   ,  <<"secret">>
   ,  #{
         <<"security">> => <<"confidential">>,
         <<"redirect_uri">> => <<"https://example.com/confidential">>
      }
   ].

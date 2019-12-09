-module(oauth2_FIXTURES).

-export([
   client_default/0
,  client_public/0
,  client_confidential/0
,  joe/0
]).

%%
client_default() ->
   [
      {iri, <<"Jaba3STOV1Ioz1GrJ-W_wA">>, <<"account@oauth2">>}
   ,  undefined
   ,  #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"https://example.com/oauth2/account">>
      }
   ].

%%
client_public() ->
   [
      {iri, <<"mz_riE1VVY7WvBJLdbDygw">>, <<"public@org">>}
   ,  <<"secret">>
   ,  #{
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"https://example.com/public">>
      }
   ].

%%
client_confidential() ->
   [
      {iri, <<"KdGtZCcUFSHGwNOWY7deAg">>, <<"confidential@org">>}
   ,  <<"secret">>
   ,  #{
         <<"security">> => <<"confidential">>,
         <<"redirect_uri">> => <<"https://example.com/confidential">>
      }
   ].

%%
joe() ->
   [
      {iri, <<"lxQBL4BKinfHjtnPvWIONw">>, <<"joe@org">>}
   ,  <<"secret">>
   ,  #{
         <<"rd">> => <<"api">>,
         <<"wr">> => <<"ddb">>
      }
   ].

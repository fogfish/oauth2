-module(oauth2_fixtures).

-export([
   client_public/0,
   client_confidential/0,
   account/0,
   code/1,
   access_token/1,
   access_token_client/1,
   refresh_token/1,
   refresh_token_client/1
]).


client_public() ->
   {
      <<"client.p">>, 
      <<"nosecret">>,
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"public">>,
         <<"redirect_uri">> => <<"http://example.com/path">>,
         <<"master">> => <<"account.a">>
      }
   }.

client_confidential() ->
   {
      <<"client.c">>, 
      <<"nosecret">>, 
      #{
         <<"type">> => <<"oauth2:client">>,
         <<"security">> => <<"confidential">>,
         <<"redirect_uri">> => <<"http://example.com/path">>,
         <<"master">> => <<"account.a">>
      }
   }.

account() ->
   {
      <<"account.a">>, 
      <<"nosecret">>,
      #{
         <<"type">> => <<"oauth2:account">>,
         <<"uid">> => true
      }
   }.

code(Access) ->
   #{
      <<"iss">> => <<"http://localhost:8080">>,
      <<"aud">> => <<"any">>,
      <<"sub">> => Access,
      <<"exch">>=> true
   }.

access_token(Access) ->
   #{
      <<"iss">> => <<"http://localhost:8080">>,
      <<"aud">> => <<"any">>,
      <<"sub">> => Access,
      <<"rev">> => true,
      <<"uid">> => true
   }.

access_token_client(Access) ->
   #{
      <<"iss">> => <<"http://localhost:8080">>,
      <<"aud">> => <<"any">>,
      <<"idp">> => <<"account.a">>,
      <<"sub">> => Access,
      <<"rev">> => true,
      <<"uid">> => true
   }.

refresh_token(Access) ->
   #{
      <<"iss">> => <<"http://localhost:8080">>,
      <<"aud">> => <<"any">>,
      <<"sub">> => Access,
      <<"rev">> => true,
      <<"exch">> => true
   }.

refresh_token_client(Access) ->
   #{
      <<"iss">> => <<"http://localhost:8080">>,
      <<"aud">> => <<"any">>,
      <<"idp">> => <<"account.a">>,
      <<"sub">> => Access,
      <<"rev">> => true,
      <<"exch">> => true
   }.   

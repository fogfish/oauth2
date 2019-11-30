-module(token).

-compile({parse_transform, category}).
-compile({parse_transform, generic}).

-export([main/1]).

%%
%%
main(Opts) ->
   {ok, _} = application:ensure_all_started(?MODULE),
   serverless:spawn(fun api/1, Opts).

%%
%%
-spec api(map()) -> datum:either(map()).

api(#{
   <<"httpMethod">> := <<"POST">>,
   <<"path">> := <<"/oauth2/token">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   } = Headers,
   <<"body">> := Request
}) ->
   serverless_api:return(oauth2:token(Headers, Request));

%%
%% https://tools.ietf.org/html/rfc7662
%%   Section 2.1. Introspection Request
%%   To prevent token scanning attacks, the endpoint MUST also require
%%   some form of authorization to access this endpoint.
%%   (The end-point is only available for confidential clients)
%%
api(#{
   <<"httpMethod">> := <<"POST">>,
   <<"path">> := <<"/oauth2/introspect">>,
   <<"headers">> := #{
      <<"content-type">>  := <<"application/x-www-form-urlencoded", _/binary>>,
      <<"Authorization">> := Digest
   },
   <<"body">> := Request
}) ->
   case
      [either ||
         oauth2_client:confidential(Digest),
         oauth2_codec:decode(Request),
         cats:optionT(badarg, lens:get(lens:at(<<"token">>, undefined), _)),
         permit:validate(_)
      ]
   of
      {ok, Claims} ->
         serverless_api:return({ok, Claims#{<<"active">> => true}});
      {error, _} ->
         serverless_api:return({ok, #{<<"active">> => false}})
   end;

api(Json) ->
   serverless:warning(Json),
   {error, not_supported}.

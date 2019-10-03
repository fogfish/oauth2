-module(authorize).

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
   <<"httpMethod">> := <<"POST">>
} = Json) ->
   case dispatch(Json) of
      {ok, Redirect} ->
         serverless_api:return({302, #{<<"Location">> => uri:s(Redirect)}, uri:s(Redirect)});
      {error, _} = Error ->
         serverless_api:return(Error)
   end;
api(_) -> 
   {error, not_supported}.

%%
%%
dispatch(#{
   <<"path">> := <<"/signin">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>,
      <<"Authorization">> := Digest
   },
   <<"body">> := Request
}) ->
   [either ||
      oauth2:auth_client_confidential(Digest),
      oauth2:signin(Request)
   ];

dispatch(#{
   <<"path">> := <<"/signin">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   },
   <<"body">> := Request
}) ->
   [either ||
      #{<<"client_id">> := Client} =< oauth2_codec:decode(Request),
      oauth2:auth_client_public(Client),
      oauth2:signin(Request)
   ];

dispatch(#{
   <<"path">> := <<"/signup">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>,
      <<"Authorization">> := Digest
   },
   <<"body">> := Request
}) ->
   [either ||
      oauth2:auth_client_confidential(Digest),
      oauth2:signup(Request)
   ];

dispatch(#{
   <<"path">> := <<"/signup">>,
   <<"headers">> := #{
      <<"Content-Type">>  := <<"application/x-www-form-urlencoded", _/binary>>
   },
   <<"body">> := Request
}) ->
   [either ||
      #{<<"client_id">> := Client} =< oauth2_codec:decode(Request),
      oauth2:auth_client_public(Client),
      oauth2:signup(Request)
   ];

dispatch(_) ->
   {error, not_supported}.

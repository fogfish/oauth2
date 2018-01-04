%%
%% @doc
%%   GitHub IAM
-module(github).
-compile({parse_transform, category}).

-export([
   auth_url/0,
   account/1
]).

%%
%%
-spec auth_url() -> uri:uri().

auth_url() ->
   uri:s(
      uri:q(
         [
            {client_id, opts:val(access_key, github)},
            {scope,     opts:val(scopes, github)}
         ],
         url(oauth_url)
      )
   ).

%%
%%
-spec account(_) -> datum:either(_).


account(Url) ->
   [either ||
      %% authenticate client application
      cats:optionT(badarg, uri:q(<<"state">>, undefined, Url)),
      Client <- oauth2_client:lookup(_),
      oauth2_client:is_public(Client),

      %% exchange code with github
      cats:optionT(badarg, uri:q(<<"code">>, undefined, Url)),
      access_token(_),
      access_profile(_),
      create_account(Client, _)
   ].

%%
%%
access_token(Code) ->
   [either ||
      Sock <- knet:socket(url(token_url), [{active, true}]),
      Data <- cats:unit([either ||
         Req <- access_token_request(Code),
         knet:send(Sock, {'POST', url(token_url), [{<<"Connection">>, <<"keep-alive">>}, {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}, {<<"Transfer-Encoding">>, <<"chunked">>}]}),
         knet:send(Sock, {packet, Req}),
         knet:send(Sock, eof),
         cats:unit(stream:list(knet:stream(Sock))),
         htcodec:decode(_),
         cats:unit(lens:get(lens:at(<<"access_token">>), _))
      ]),
      knet:close(Sock),
      cats:flatten(Data)
   ].

access_token_request(Code) ->
   htcodec:encode(<<"application/x-www-form-urlencoded">>, 
      #{
         client_id => opts:val(access_key, github),
         client_secret => opts:val(secret_key, github),
         code => Code
      }
   ).

%%
%%
access_profile(Token) ->
   [either ||
      Sock  <- knet:socket(url(ghapi_url), [{active, true}]),
      Data  <- cats:unit([either ||
         knet:send(Sock, {'GET', url(ghapi_url), [{<<"Connection">>, <<"keep-alive">>}, {<<"Authorization">>, <<"Bearer ", Token/binary>>}, {<<"Accept">>, <<"application/json">>}, {<<"User-Agent">>, <<"knet">>}]}),
         knet:send(Sock, eof),
         cats:unit(stream:list(knet:stream(Sock))),
         htcodec:decode(_),
         Access <- cats:unit(lens:get(lens:at(<<"login">>), _)),

         knet:send(Sock, {'GET', uri:join([orgs], url(ghapi_url)), [{<<"Connection">>, <<"keep-alive">>}, {<<"Authorization">>, <<"Bearer ", Token/binary>>}, {<<"Accept">>, <<"application/json">>}, {<<"User-Agent">>, <<"knet">>}]}),
         knet:send(Sock, eof),
         cats:unit(stream:list(knet:stream(Sock))),
         htcodec:decode(_),
         Master  <- org(_),
         cats:unit(#{access => Access, master => Master})
      ]),
      knet:close(Sock),
      cats:flatten(Data)
   ].


%%
%%
org(List) ->
   case
      lens:get(
         lens:c(lens:takewith(fun allowed_org/1, undefined), lens:at(<<"login">>)),
         List
      )
   of
      undefined ->
         {error, unauthorized};

      Org ->
         {ok, Org}
   end.

allowed_org(#{<<"login">> := Org}) ->
   Org =:= scalar:s(opts:val(org, github)).


%%
%%
create_account(Client, #{access := Access, master := Master}) ->
   Secret = crypto:strong_rand_bytes(30),
   case permit:lookup(Access) of
      {error, not_found} ->
         %% token here
         permit:create(Access, Secret, #{
            <<"type">>   => <<"oauth2:account">>,
            <<"uid">>    => true,
            <<"master">> => Master
         });

      {ok, _} ->
         permit:update(Access, Secret, #{
            <<"type">>   => <<"oauth2:account">>,
            <<"uid">>    => true,
            <<"master">> => Master
         })
   end,
   case 
      oauth2_token:exchange_code(Access, Secret)
   of
      {ok, Code} ->
         redirect_uri([{code, Code}], Client);
      {error, _} = Error ->
         redirect_uri([Error], Client)
   end.


%%
%%
url(Key) ->
   uri:new(opts:val(Key, github)).

%%
redirect_uri(Status, Client) ->
   oauth2_client:redirect_uri(Client, Status).

